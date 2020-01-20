{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module Clock where

import Control.Monad
import Data.IORef
import Data.Time.Calendar
import Data.Time.Clock.System
import Data.Time.Format
import Data.Time.LocalTime
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Text.Printf
import Text.Read

import Netlist
import Simulator

wordSize = 42

data Options = Async | InitTime String
             deriving Eq

options = [ Option ['a'] ["async"] (NoArg Async) "run in async mode"
          , Option ['i'] ["init"] (ReqArg InitTime "TIMEDATE") "initial timedate"
          ]

usage = "usage: clock [OPTIONS...] NETLIST"

main = do
    -- Process command-line arguments
    args <- getArgs
    (opts, f) <- case getOpt Permute options args of
        (o, [f], []) -> return (o, f)
        (_, _, errs) -> die $ init $ concat errs ++ usageInfo usage options
    let async = Async `elem` opts

    -- Read and parse the netlist
    netlist@Netlist{..} <- readNetlist f

    -- Create a machine
    m <- newMachine netlist
    let readInteger a = bitsToInteger <$> readRam m wordSize a
        writeInteger a i = writeRam m wordSize a (bitsFromInteger wordSize i)

    -- Initialise the time
    LocalTime day' (TimeOfDay hour minute second) <-
        case [lt | InitTime lt <- opts] of
            lt:_ -> parseTimeM True defaultTimeLocale "%Y-%-m-%-d %-H:%-M:%-S" lt
            _ -> zonedTimeToLocalTime <$> getZonedTime
    let (year, month, day) = toGregorian day'
    zipWithM_ writeInteger [1024..1029] [floor second, minute, hour, day, month, fromInteger year]

    -- Run the simulation
    let getValue x s = return (replicate s False)
        getSystemSeconds = systemSeconds <$> getSystemTime
    lastSecond <- newIORef =<< getSystemSeconds
    forever do
        -- Update the time difference
        if async then
            writeInteger 1030 1
        else do
            s <- getSystemSeconds
            s' <- readIORef lastSecond
            writeIORef lastSecond s
            t <- readInteger 1030
            writeInteger 1030 (t + s - s')

        -- Run one step
        runStep m getValue

        -- Print the time
        [second, minute, hour, day, month, year] <-
            mapM readInteger [1024..1029] :: IO [Integer]
        printf "\r%04d-%02d-%02d %02d:%02d:%02d" year month day hour minute second
        hFlush stdout
