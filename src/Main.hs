{-# LANGUAGE TupleSections #-}
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

usage = do
    progName <- getProgName
    die $ "usage: " ++ progName ++ " [-n steps] netlist"

getValue x s = f where
    f = do
        printf "%s ? " x
        hFlush stdout
        v <- readMaybe @Value <$> getLine
        case v of
            Just (V l) | length l == s -> return l
            _ -> putStrLn "Wrong input." >> f

nand x y = not (x && y)

slice i j l = take (j - i + 1) (drop i l)

bitFromBool = bool zeroBits (bit 0)

bitsFromListBE = foldl (\n b -> (n `shiftL` 1) .|. bitFromBool b) zeroBits

bitsToListBE b = let s = finiteBitSize b in [testBit b i | i <- [s - 1, s - 2..0]]

listBEFromFile f = either (const []) (\b -> B.unpack b >>= bitsToListBE) <$>
    tryJust (guard . isDoesNotExistError) (B.readFile f)

main = do
    args <- getArgs
    (steps, f) <- maybe usage return $ case args of
        ["-n", n, f] -> (\n -> ([1..n], f)) <$> readMaybe @Integer n
        [f] -> Just ([1..], f)
        _ -> Nothing
    netlist <- either die return . readEither @Netlist =<< readFile f
    let ramSize = maximum $ 0:[s * 2^a | (_, Eram a s _ _ _ _) <- equations netlist]
        romSize = maximum $ 0:[s * 2^a | (_, Erom a s _) <- equations netlist]
        varSize x = fromJust $ lookup x $ vars netlist
    ramBits <- listBEFromFile "ram.bin"
    ram <- newListArray @IOUArray (0, ramSize - 1) (ramBits ++ repeat False)
    romBits <- listBEFromFile "rom.bin"
    let rom = listArray @UArray (0, romSize - 1) (romBits ++ repeat False)
    env <- newIORef $ M.fromList [(x, replicate s False) | (x, s) <- vars netlist]
    for_ steps $ \i -> do
        printf "Step %d:\n" i
        oldEnv <- readIORef env
        for_ (invars netlist) $ \x -> do
            v <- getValue x (varSize x)
            modifyIORef' env $ M.insert x v
        for_ (equations netlist) $ \(x, ex) -> do
            e <- readIORef env
            let arg (Avar x) = e M.! x
                arg (Aconst (V l)) = l
                address s a = s * bitsFromListBE (arg a)
            v <- case ex of
                Eram a s ra we wa w -> do
                    rv <- sequence [readArray ram (address s ra + i) | i <- [0..s - 1]]
                    when (or (arg we)) $
                        sequence_ [writeArray ram (address s wa + i) v | (i, v) <- zip [0..s - 1] (arg w)]
                    return rv
                _ -> return $ case ex of
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
        e <- readIORef env
        sequence_ [printf "=> %s = " x >> print (V (e M.! x)) | x <- outvars netlist]
