{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
module Simulator where

import Control.Monad
import System.Environment
import System.Exit
import System.IO
import Text.Printf
import Text.Read

import Netlist
import Machine

usage = do
    progName <- getProgName
    die $ "usage: " ++ progName ++ " [-n steps] netlist"

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
    netlist@Netlist{..} <- either die pure . readEither @Netlist =<< readFile f
    ramBits <- listBEFromFile "ram.bin"
    romBits <- listBEFromFile "rom.bin"
    m <- newMachine netlist ramBits romBits
    forM_ steps \i -> do
        printf "Step %d:\n" i
        runStep m getValue
        forM_ outvars \x -> do
            printf "=> %s = " x
            v <- getVariable m x
            print v
