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
import Data.Foldable
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import Data.Word
import System.Environment
import System.Exit
import System.IO
import System.IO.Error
import Text.Printf
import Text.Read

import Netlist

-- Utility stuff

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

usage = do
    progName <- getProgName
    die $ "usage: " ++ progName ++ " [-n steps] netlist"

-- Get a value from standard input
getValue x s = f where
    f = do
        printf "%s ? " x
        hFlush stdout
        v <- readMaybe @Value <$> getLine
        case v of
            Just (Value l) | length l == s -> return l
            _ -> putStrLn "Wrong input." >> f

-- Main action

main = do
    -- Process command-line arguments
    args <- getArgs
    (steps, f) <- maybe usage pure case args of
        ["-n", n, f]
            | Just n' <- readMaybe @Integer n -> Just ([1..n'], f)
        [f] -> Just ([1..], f)
        _ -> Nothing
    -- Read and parse the netlist
    Netlist{..} <- either die pure . readEither @Netlist =<< readFile f
    -- ROM and RAM
    let romSize = 42 * 32768
        ramSize = 42 * 32768
    romBits <- listBEFromFile "rom.bin"
    let rom = listArray @UArray (0, romSize - 1) (romBits ++ repeat False)
    ramBits <- listBEFromFile "ram.bin"
    ram <- newListArray @IOUArray (0, ramSize - 1) (ramBits ++ repeat False)
    env <- newIORef $ M.fromList [(x, replicate s False) | (x, s) <- vars]
    computed <- newIORef S.empty
    let readRam s ra    = mapM (readArray ram) [ra..ra + s - 1]
        writeRam s wa w = zipWithM_ (writeArray ram) [wa..wa + s - 1] w
        readRom s ra    = map (rom !) [ra..ra + s - 1]
        arg (Aconst (Value l)) = return l
        arg (Avar x)           = compute x
        address s a = (s *) . bitsFromListBE <$> arg a
        compute x = do
            c <- S.member x <$> readIORef computed
            unless c do
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
                    Erom a s ra  -> readRom s <$> address a ra
                    Eram a s ra we wa w -> do
                        r <- readRam s =<< address a ra
                        we <- or <$> arg we
                        when we do
                            join $ writeRam s <$> address a wa <*> arg w
                        return r
                modifyIORef' env $ M.insert x v
                modifyIORef' computed $ S.insert x
            (M.! x) <$> readIORef env
    -- Simulate
    let ramvars = [x | (x, Eram{}) <- M.assocs equations]
        regvars = [x | (_, Ereg x) <- M.assocs equations]
    forM_ steps \i -> do
        printf "Step %d:\n" i
        forM_ invars \x -> do
            let Just s = lookup x vars
            v <- getValue x s
            modifyIORef' env $ M.insert x v
        writeIORef computed S.empty
        forM_ (ramvars ++ regvars) compute
        forM_ outvars \x -> do
            v <- compute x
            printf "=> %s = " x
            print (Value v)
        forM_ [1024..1026 :: Int] \a -> do
            v <- bitsFromListBE <$> readRam 42 (42 * a)
            printf "RAM %d = %d\n" a v
