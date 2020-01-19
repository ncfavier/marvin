{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
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
import Text.Megaparsec.Char hiding (space)
import Text.Megaparsec.Char.Lexer
import System.Environment
import System.Exit

instance (Stream s, MonadWriter w m) => MonadWriter w (ParsecT e s m) where
    tell = lift . tell
    listen = undefined
    pass = undefined

data Operand = Int Integer | Var String

data Environment = Environment { vars :: Map String Integer
                               , pos  :: Integer
                               }

initialEnvironment = Environment M.empty 0

type Parser = ParsecT Void String (WriterT [Operand] (State Environment))

usage = do
    progName <- getProgName
    die $ "usage: " ++ progName ++ " file"

main = do
    args <- getArgs
    f <- maybe usage pure case args of
        [f] -> Just f
        _ -> Nothing
    input <- readFile f
    let ((r, output), Environment { vars }) = flip runState initialEnvironment
                                            $ runWriterT
                                            $ runParserT file f
                                            $ input
    either (die . errorBundlePretty) pure r
    let printOperand (Int i) = print i
        printOperand (Var v) = print (vars M.! v)
    mapM_ printOperand output

whitespace = space (skipSome (oneOf " \t")) (skipLineComment ";") empty

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

operand = Int <$> integer <|> Var <$> ident

instruction = do
    ins <- ident
    ops <- many operand
    emit (Int (code M.! ins):ops)

[ a_in,
  a_out,
  ram_in,
  ram_out,
  add_out,
  sub,
  jump,
  zero,
  less
  ] = bit <$> [0..8]

code = M.fromList
    [ ("load",  ram_out .|. a_in)
    , ("store", a_out .|. ram_in)
    , ("add",   add_out .|. a_in)
    , ("sub",   add_out .|. sub .|. a_in)
    , ("jump",  jump)
    , ("jz",    jump .|. zero)
    , ("jl",    jump .|. less)
    ]
