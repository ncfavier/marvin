{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Clock where

import Control.Exception
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
import System.IO.Error
import Text.Printf
import Text.Read

import Netlist
import Machine

wordSize = 42

bitsFromFile f =  foldMap (bitsFromInteger wordSize)
               .  either (const []) (map (read @Integer) . lines)
              <$> tryJust (guard . isDoesNotExistError) (readFile f)

data Options = Async | InitTime String
             deriving Eq

options = [ Option ['a'] ["async"] (NoArg Async) "run in async mode"
          , Option ['i'] ["init"] (ReqArg InitTime "TIMEDATE") "initial timedate"
          ]

main = do
    args <- getArgs
    (opts, f) <- case getOpt Permute options args of
        (o, [f], []) -> return (o, f)
        (_, _, errs) -> die $ concat errs ++ usageInfo "usage: clock [OPTIONS...] NETLIST" options
    let async = Async `elem` opts
    netlist@Netlist{..} <- either die pure . readEither @Netlist =<< readFile f
    ramBits <- bitsFromFile "ram.bin"
    romBits <- bitsFromFile "rom.bin"
    m <- newMachine netlist ramBits romBits
    let readInteger a = bitsToInteger <$> readRam m wordSize (wordSize * a)
        writeInteger a i = writeRam m wordSize (wordSize * a) (bitsFromInteger wordSize (toInteger i))
    LocalTime day' (TimeOfDay hour minute second) <-
        case [lt | InitTime lt <- opts] of
            lt:_ -> parseTimeM True defaultTimeLocale "%Y-%-m-%-d %-H:%-M:%-S" lt
            _ -> zonedTimeToLocalTime <$> getZonedTime
    let (year, month, day) = toGregorian day'
    zipWithM_ writeInteger [1024..1029] [floor second, minute, hour, day, month, fromIntegral year]
    let getValue x s = return (replicate s False)
        getSystemSeconds = systemSeconds <$> getSystemTime
    lastSecond <- newIORef =<< getSystemSeconds
    forever do
        pulse <- if async then return True else do
            s <- getSystemSeconds
            s' <- readIORef lastSecond
            if s > s' then True <$ writeIORef lastSecond s
            else return False
        when pulse $ writeInteger 1030 1
        runStep m getValue
        [second, minute, hour, day, month, year] <-
            mapM readInteger [1024..1029]
        printf "\r%04d-%02d-%02d %02d:%02d:%02d" year month day hour minute second
        hFlush stdout
