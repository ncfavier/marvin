{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
import Control.Exception
import Control.Monad
import Data.Array.IArray
import Data.Array.IO
import Data.Array.MArray
import Data.Array.Unboxed
import Data.Bits
import Data.Bool
import qualified Data.ByteString.Lazy as B
import Data.Foldable
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe
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

bitsToListBE b = [testBit b i | i <- [s - 1, s - 2..0]]
    where s = finiteBitSize b

listBEFromFile f = either (const []) (\b -> B.unpack b >>= bitsToListBE) <$>
    tryJust (guard . isDoesNotExistError) (B.readFile f)

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
    netlist <- either die pure . readEither @Netlist =<< readFile f
    -- Compute ROM and RAM size
    let romSize = maximum $ 0:[s * 2^a | (_, Erom a s _) <- equations netlist]
        ramSize = maximum $ 0:[s * 2^a | (_, Eram a s _ _ _ _) <- equations netlist]
    -- Make ROM and RAM
    romBits <- listBEFromFile "rom.bin"
    let rom = listArray @UArray (0, romSize - 1) (romBits ++ repeat False)
    ramBits <- listBEFromFile "ram.bin"
    ram <- newListArray @IOUArray (0, ramSize - 1) (ramBits ++ repeat False)
    -- Make the environment
    env <- newIORef $ M.fromList [(x, replicate s False) | (x, s) <- vars netlist]
    -- Simulate
    for_ steps \i -> do
        printf "Step %d:\n" i
        -- Keep the old environment around for registers
        oldEnv <- readIORef env
        -- Read input variables
        for_ (invars netlist) \x -> do
            v <- getValue x (fromJust $ lookup x $ vars netlist)
            modifyIORef' env $ M.insert x v
        -- Compute equations
        for_ (equations netlist) \(x, ex) -> do
            e <- readIORef env
            -- Helpers
            let arg (Avar x) = e M.! x
                arg (Aconst (Value l)) = l
                address s a = s * bitsFromListBE (arg a)
            v <- case ex of
                -- RAM access is the only kind of expression whose evaluation requires I/O
                Eram a s ra we wa w -> do
                    -- Read from RAM
                    rv <- sequence [readArray ram (address s ra + i) | i <- [0..s - 1]]
                    -- If `we` is nonzero, write to RAM
                    when (or (arg we)) do
                        sequence_ [writeArray ram (address s wa + i) v | (i, v) <- zip [0..s - 1] (arg w)]
                    return rv
                -- Everything else is pure
                _ -> pure case ex of
                    Earg a       -> arg a
                    Ereg x       -> oldEnv M.! x
                    Enot a       -> map not (arg a)
                    Eor a b      -> zipWith (||) (arg a) (arg b)
                    Exor a b     -> zipWith (/=) (arg a) (arg b)
                    Eand a b     -> zipWith (&&) (arg a) (arg b)
                    Enand a b    -> zipWith nand (arg a) (arg b)
                    Emux s a b   -> bool (arg a) (arg b) (or (arg s))
                    Econcat a b  -> arg a ++ arg b
                    Eslice i j a -> slice i j (arg a)
                    Eselect i a  -> slice i i (arg a)
                    Erom a s ra  -> [rom ! (address s ra + i) | i <- [0..s - 1]]
            writeIORef env $ M.insert x v e
        -- Print output variables
        e <- readIORef env
        for_ (outvars netlist) \x -> do
            printf "=> %s = " x
            print (Value (e M.! x))
