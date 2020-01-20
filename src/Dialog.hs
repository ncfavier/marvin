{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
module Dialog where

import Control.Monad
import System.Environment
import System.Exit
import System.IO
import Text.Printf
import Text.Read

import Netlist
import Simulator

usage = do
    progName <- getProgName
    die $ "usage: " ++ progName ++ " [-n STEPS] NETLIST"

getValue x s = go where
    go = do
        printf "%s ? " x
        hFlush stdout
        v <- readMaybe @Value <$> getLine
        case v of
            Just (Value l) | length l == s -> return l
            _ -> putStrLn "Wrong input." >> go

main = do
    args <- getArgs
    (steps, f) <- maybe usage pure case args of
        ["-n", n, f]
            | Just n' <- readMaybe @Integer n -> Just ([1..n'], f)
        [f] -> Just ([1..], f)
        _ -> Nothing
    netlist@Netlist{..} <- readNetlist f
    m <- newMachine netlist getValue
    forM_ steps \i -> do
        printf "Step %d:\n" i
        runStep m
        mapM_ (printVariable m) outvars
