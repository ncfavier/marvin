{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Simulator where

import Control.Exception
import Control.Monad
import Data.Array.IArray
import Data.Array.IO
import Data.Array.MArray
import Data.Array.Unboxed
import Data.Bool
import Data.Bits
import Data.IORef
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import System.IO
import System.IO.Error
import Text.Printf

import Netlist

nand x y = not (x && y)

slice i j l = take (j - i + 1) (drop i l)

bitsToInteger :: Bits i => [Bool] -> i
bitsToInteger = foldl f zeroBits
    where f n b = (n `shiftL` 1) .|. bool zeroBits (bit 0) b

bitsFromInteger :: (Bits i, Integral j) => j -> i -> [Bool]
bitsFromInteger s n = [testBit n i | i <- [s' - 1, s' - 2..0]]
    where s' = fromIntegral s

whenM c a = do c <- c; when c a

data Machine = Machine { netlist  :: Netlist
                       , ram      :: IOUArray Int Bool
                       , rom      :: UArray Int Bool
                       , env      :: IORef (Map Variable [Bool])
                       , oldEnv   :: IORef (Map Variable [Bool])
                       , roots    :: [Variable]
                       }

readImage f = do
    ns <- either (const []) (map (read @Integer) . lines) <$>
        tryJust (guard . isDoesNotExistError) (readFile f)
    return case ns of
        wordSize:size:ns' ->
            (foldMap (bitsFromInteger wordSize) ns', fromIntegral (wordSize * size))
        _ -> ([], 0)

newMachine netlist@Netlist{..} = do
    (ramBits, ramSize) <- readImage "ram.img"
    ram <- newListArray @IOUArray (0, ramSize - 1) (ramBits ++ repeat False)
    (romBits, romSize) <- readImage "rom.img"
    let rom = listArray @UArray (0, romSize - 1) (romBits ++ repeat False)
    env <- newIORef M.empty
    oldEnv <- newIORef $ M.map (\s -> replicate s False) vars
    let ramvars = [x | (x, Eram{}) <- M.assocs equations]
        regvars = [x | (_, Ereg x) <- M.assocs equations]
        roots = nub (ramvars ++ regvars ++ invars ++ outvars)
    return Machine{..}

getVariable m@Machine{..} x = do
    computed <- M.member x <$> readIORef env
    unless computed $ compute m x
    (M.! x) <$> readIORef env

printVariable m@Machine{..} x = do
    v <- getVariable m x
    printf "==> %s = %d\n" x (bitsToInteger v :: Integer)

readRam Machine{..} s ra = mapM (readArray ram) [ra*s..(ra + 1)*s - 1]

writeRam Machine{..} s wa w = zipWithM_ (writeArray ram) [wa*s..(wa + 1)*s - 1] w

readRom Machine{..} s ra = map (rom !) [ra*s..(ra + 1)*s - 1]

compute m@Machine{ netlist = Netlist{..}, .. } x = do
    v <- case equations M.! x of
        Earg a       -> arg a
        Ereg x       -> (M.! x) <$> readIORef oldEnv
        Enot a       -> map not <$> arg a
        Eor a b      -> zipWith (||) <$> arg a <*> arg b
        Exor a b     -> zipWith (/=) <$> arg a <*> arg b
        Eand a b     -> zipWith (&&) <$> arg a <*> arg b
        Enand a b    -> zipWith nand <$> arg a <*> arg b
        Emux s a b   -> bool (arg b) (arg a) . or =<< arg s
        Econcat a b  -> (++) <$> arg a <*> arg b
        Eslice i j a -> slice i j <$> arg a
        Eselect i a  -> slice i i <$> arg a
        Erom _ s ra  -> readRom m s . bitsToInteger <$> arg ra
        Eram _ s ra we wa w -> do
            r <- readRam m s . bitsToInteger =<< arg ra
            r <$ whenM (or <$> arg we) do
                join $ writeRam m s . bitsToInteger <$> arg wa <*> arg w
    modifyIORef' env $ M.insert x v
    where
        arg (Aconst (Value l)) = return l
        arg (Avar x)           = getVariable m x

runStep m@Machine{ netlist = Netlist{..}, .. } getValue = do
    writeIORef env M.empty
    forM_ invars \x -> do
        v <- getValue x (vars M.! x)
        modifyIORef' env $ M.insert x v
    forM_ roots \x -> getVariable m x
    writeIORef oldEnv =<< readIORef env
