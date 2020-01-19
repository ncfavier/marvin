{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Machine where

import Control.Exception
import Control.Monad
import Data.Array.IArray
import Data.Array.IO
import Data.Array.MArray
import Data.Array.Unboxed
import Data.Bits
import Data.Bool
import Data.IORef
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import System.IO
import System.IO.Error

import Netlist

nand x y = not (x && y)

slice i j l = take (j - i + 1) (drop i l)

bitFromBool = bool zeroBits (bit 0)

bitsFromListBE = foldl (\n b -> (n `shiftL` 1) .|. bitFromBool b) zeroBits

finiteBitsToListBE b = bitsToListBE s b
    where s = finiteBitSize b

bitsToListBE s b = [testBit b i | i <- [s - 1, s - 2..0]]

listBEFromFile f = foldMap (bitsToListBE 42)
                 . either (const []) (map (read @Integer) . lines)
               <$> tryJust (guard . isDoesNotExistError) (readFile f)

data Machine = Machine { netlist  :: Netlist
                       , ram      :: IOUArray Int Bool
                       , rom      :: UArray Int Bool
                       , env      :: IORef (Map Variable [Bool])
                       , computed :: IORef (Set Variable)
                       , roots    :: [Variable]
                       }

ramSize = 2^20
romSize = 2^10

newMachine netlist@Netlist{..} ramBits romBits = do
    ram <- newListArray @IOUArray (0, ramSize - 1) (ramBits ++ repeat False)
    let rom = listArray @UArray (0, romSize - 1) (romBits ++ repeat False)
    env <- newIORef $ M.map (\s -> replicate s False) vars
    computed <- newIORef S.empty
    let ramvars = [x | (x, Eram{}) <- M.assocs equations]
        regvars = [x | (_, Ereg x) <- M.assocs equations]
        roots = nub (ramvars ++ regvars)
    return Machine{..}

getVariable m@Machine{..} x = do
    computed <- S.member x <$> readIORef computed
    unless computed $ compute m x
    (M.! x) <$> readIORef env

readRam Machine{..} s ra = mapM (readArray ram) [ra..ra + s - 1]

writeRam Machine{..} s wa w = zipWithM_ (writeArray ram) [wa..wa + s - 1] w

readRom Machine{..} s ra = map (rom !) [ra..ra + s - 1]

compute m@Machine{ netlist = Netlist{..}, .. } x = do
    let arg (Aconst (Value l)) = return l
        arg (Avar x)           = getVariable m x
        address s a = (s *) . bitsFromListBE <$> arg a
    v <- case equations M.! x of
        Earg a       -> arg a
        Ereg x       -> (M.! x) <$> readIORef env
        Enot a       -> map not <$> arg a
        Eor a b      -> zipWith (||) <$> arg a <*> arg b
        Exor a b     -> zipWith (/=) <$> arg a <*> arg b
        Eand a b     -> zipWith (&&) <$> arg a <*> arg b
        Enand a b    -> zipWith nand <$> arg a <*> arg b
        Emux s a b   -> bool (arg a) (arg b) . or =<< arg s
        Econcat a b  -> (++) <$> arg a <*> arg b
        Eslice i j a -> slice i j <$> arg a
        Eselect i a  -> slice i i <$> arg a
        Erom a s ra  -> readRom m s <$> address a ra
        Eram a s ra we wa w -> do
            r <- readRam m s =<< address a ra
            we <- or <$> arg we
            when we do
                join $ writeRam m s <$> address a wa <*> arg w
            return r
    modifyIORef' env $ M.insert x v
    modifyIORef' computed $ S.insert x

runStep m@Machine{ netlist = Netlist{..}, .. } getValue = do
    writeIORef computed S.empty
    forM_ invars \x -> do
        v <- getValue x (vars M.! x)
        modifyIORef' env $ M.insert x v
    forM_ roots \x -> getVariable m x
