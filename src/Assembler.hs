{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Assembler where

import Prelude hiding (lex)
import Control.Monad.State
import Control.Monad.Writer
import Data.Bits
import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Void
import Text.Megaparsec hiding (State, label)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)
import System.Environment
import System.Exit

instance (Stream s, MonadWriter w m) => MonadWriter w (ParsecT e s m) where
    tell = lift . tell
    listen = undefined
    pass = undefined

data Environment = Environment { vars :: Map String Integer
                               , pos  :: Integer
                               }

initialEnvironment = Environment M.empty 0

type Parser = ParsecT Void String (WriterT [Integer] (State Environment))

usage = do
    progName <- getProgName
    die $ "usage: " ++ progName ++ " file"

main = do
    args <- getArgs
    f <- maybe usage pure case args of
        [f] -> Just f
        _ -> Nothing
    input <- readFile f
    let (r, output) = flip evalState initialEnvironment
                    $ runWriterT
                    $ runParserT file f
                    $ input
    either (die . errorBundlePretty) pure r
    mapM_ print output

whitespace = skipMany (oneOf " \t")

lex :: Parser a -> Parser a
lex = lexeme whitespace

sym :: String -> Parser String
sym = symbol whitespace

integer = lex decimal

ident = lex $ some $ satisfy (\c -> isLetter c || c == '_')

emit l = do
    tell l
    modify' $ \e -> e { pos = pos e + genericLength l }

file :: Parser ()
file = do
    many (line >> eol)
    eof

line = do
    whitespace
    optional $ try assignment <|> try label <|> instruction

assignment = do
    x <- ident
    sym "="
    v <- integer
    modify' $ \e -> e { vars = M.insert x v (vars e) }

label = do
    l <- ident
    sym ":"
    modify' $ \e -> e { vars = M.insert l (pos e) (vars e) }

variable = do
    v <- ident
    vars <- gets vars
    return $ vars M.! v

operand = integer <|> variable

instruction = do
    ins <- ident
    ops <- many operand
    emit (code M.! ins:ops)

[loadBit, storeBit, addBit, subBit, jumpBit, jlBit] = bit <$> [0..5]

code = M.fromList
    [ ("load",  loadBit)
    , ("store", storeBit)
    , ("add",   addBit)
    , ("sub",   addBit .|. subBit)
    , ("jump",  jumpBit)
    , ("jl",    jumpBit .|. jlBit)
    ]
