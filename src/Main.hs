{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
import Control.Monad
import Data.Array.IArray
import Data.Array.IO
import Data.Array.MArray
import Data.Array.Unboxed
import Data.Bool
import Data.Foldable
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe
import System.Environment
import System.Exit
import System.IO
import Text.Printf
import Text.Read

import Netlist

usage = do
    progName <- getProgName
    die $ "usage: " ++ progName ++ " [-n steps] file"

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

fromBits :: [Bool] -> Int
fromBits = foldl (\n b -> 2*n + fromEnum b) 0

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
    ram <- newArray @IOUArray (0, ramSize - 1) False
    let rom = listArray @UArray (0, romSize - 1) (repeat False)
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
            v <- case ex of
                Eram a s ra we wa w -> do
                    let ra' = s * fromBits (arg ra)
                    rv <- sequence [readArray ram (ra' + i) | i <- [0..s - 1]]
                    when (or (arg we)) $ do
                        let wa' = s * fromBits (arg wa)
                        sequence_ [writeArray ram (wa' + i) v | (i, v) <- zip [0..s - 1] (arg w)]
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
                    Erom a s ra  -> let ra' = s * fromBits (arg ra) in
                                    [rom ! (ra' + i) | i <- [0..s - 1]]
            writeIORef env $ M.insert x v e
        e <- readIORef env
        sequence_ [printf "=> %s = " x >> print (V (e M.! x)) | x <- outvars netlist]
