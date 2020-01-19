{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
module Clock where

import Control.Monad
import Data.IORef
import Data.Time.Calendar
import Data.Time.Clock.System
import Data.Time.LocalTime
import System.Environment
import System.Exit
import System.IO
import Text.Printf
import Text.Read

import Netlist
import Machine

usage = do
    progName <- getProgName
    die $ "usage: " ++ progName ++ " [--async] netlist"

main = do
    args <- getArgs
    (async, f) <- maybe usage pure case args of
        ["--async", f] -> Just (True, f)
        [f]            -> Just (False, f)
        _ -> Nothing
    netlist@Netlist{..} <- either die pure . readEither @Netlist =<< readFile f
    ramBits <- listBEFromFile "ram.bin"
    romBits <- listBEFromFile "rom.bin"
    m <- newMachine netlist ramBits romBits
    let readInteger :: Int -> IO Int
        readInteger a = bitsFromListBE <$> readRam m 42 (42 * a)
        writeInteger :: Int -> Int -> IO ()
        writeInteger a v = writeRam m 42 (42 * a) (bitsToListBE 42 v)
    LocalTime day' (TimeOfDay hour minute second) <-
        zonedTimeToLocalTime <$> getZonedTime
    let (year, month, day) = toGregorian day'
    zipWithM_ writeInteger [1024..1029] [floor second, minute, hour, day, month, fromIntegral year]
    let getValue x s = return (replicate s False)
        getSystemSeconds = systemSeconds <$> getSystemTime
    lastSecond <- newIORef =<< getSystemSeconds
    forever do
        pulse <- if async then return True else do
            s <- getSystemSeconds
            s' <- readIORef lastSecond
            if s > s' then do
                writeIORef lastSecond s
                return True
            else return False
        when pulse $ writeInteger 1030 1
        runStep m getValue
        [second, minute, hour, day, month, year] <-
            mapM readInteger [1024..1029]
        printf "\r%02d:%02d:%02d %04d-%02d-%02d" hour minute second year month day
        hFlush stdout
