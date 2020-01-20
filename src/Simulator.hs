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
import Data.Bits
import Data.Bool
import Data.IORef
import Data.List
import Data.Maybe
import System.Exit
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

data Machine = Machine { netlist   :: Netlist
                       , ram       :: IOUArray Int Bool
                       , rom       :: UArray Int Bool
                       , env       :: IOArray Variable [Bool]
                       , states    :: IORef (IOArray Variable State)
                       , roots     :: [Variable]
                       , input     :: String -> Int -> IO [Bool]
                       }

data State = Old | InProgress | New

readImage f = do
    ns <- either (const []) (map (read @Integer) . lines) <$>
        tryJust (guard . isDoesNotExistError) (readFile f)
    return case ns of
        wordSize:size:ns' ->
            (foldMap (bitsFromInteger wordSize) ns', fromIntegral (wordSize * size))
        _ -> ([], 0)

newMachine netlist@Netlist{..} input = do
    (ramBits, ramSize) <- readImage "ram.img"
    ram <- newListArray @IOUArray (0, ramSize - 1) (ramBits ++ repeat False)
    (romBits, romSize) <- readImage "rom.img"
    let rom = listArray @UArray (0, romSize - 1) (romBits ++ repeat False)
    env <- newListArray varBounds [replicate s False | (_, s) <- elems vars]
    states <- newIORef =<< newArray varBounds Old
    let ramvars = [x | (x, Just Eram{}) <- assocs equations]
        regvars = [x | (_, Just (Ereg x)) <- assocs equations]
        roots = nub (ramvars ++ regvars ++ invars ++ outvars)
    return Machine{..}

printVariable m@Machine{ netlist = Netlist{..}, .. } x = do
    v <- getVariable m x
    let (n, _) = vars ! x
    printf "==> %s = %d\n" n (bitsToInteger v :: Integer)

readRam Machine{..} s ra = mapM (readArray ram) [ra*s..(ra + 1)*s - 1]

writeRam Machine{..} s wa w = zipWithM_ (writeArray ram) [wa*s..(wa + 1)*s - 1] w

readRom Machine{..} s ra = map (rom !) [ra*s..(ra + 1)*s - 1]

getVariable m@Machine{ netlist = Netlist{..}, .. } x = do
    states <- readIORef states
    state <- readArray states x
    case state of
        Old -> compute x
        InProgress -> do
            let (n, _) = vars ! x
            die $ "Cycle detected while computing " ++ n
        _ -> return ()
    readArray env x
    where
        arg (Aconst (Value l)) = return l
        arg (Avar x)           = getVariable m x
        address a = bitsToInteger <$> arg a
        compute x = do
            states <- readIORef states
            writeArray states x InProgress
            v <- case equations ! x of
                Just exp -> case exp of
                    Earg a       -> arg a
                    Ereg x       -> readArray env x
                    Enot a       -> map not <$> arg a
                    Eor a b      -> zipWith (||) <$> arg a <*> arg b
                    Exor a b     -> zipWith (/=) <$> arg a <*> arg b
                    Eand a b     -> zipWith (&&) <$> arg a <*> arg b
                    Enand a b    -> zipWith nand <$> arg a <*> arg b
                    Emux s a b   -> bool (arg b) (arg a) . or =<< arg s
                    Econcat a b  -> (++) <$> arg a <*> arg b
                    Eslice i j a -> slice i j <$> arg a
                    Eselect i a  -> slice i i <$> arg a
                    Erom _ s ra  -> readRom m s <$> address ra
                    Eram _ s ra we wa w -> do
                        r <- readRam m s =<< address ra
                        r <$ whenM (or <$> arg we) do
                            join $ writeRam m s <$> address wa <*> arg w
                Nothing -> do
                    let (n, s) = vars ! x
                    input n s
            writeArray env x v
            writeArray states x New

runStep m@Machine{ netlist = Netlist{..}, .. } = do
    writeIORef states =<< newArray varBounds Old
    mapM_ (getVariable m) roots
