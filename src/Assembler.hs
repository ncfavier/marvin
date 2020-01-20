{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
module Assembler where

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
import qualified Text.Megaparsec.Char.Lexer as L
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

data W = Int Integer | Var String

type Parser = ParsecT Void String (WriterT [W] (State Environment))

usage = do
    progName <- getProgName
    die $ "usage: " ++ progName ++ " FILE"

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
    print 42 -- word size
    print 2048 -- ram size
    let printWord (Int i) = print i
        printWord (Var v) = print (vars M.! v)
    mapM_ printWord output

whitespace = L.space (skipSome (oneOf " \t")) (L.skipLineComment ";") empty

lexeme = L.lexeme whitespace
symbol = L.symbol whitespace

integer = lexeme L.decimal

ident = lexeme $ some $ satisfy (\c -> isLetter c || c == '_')

emit l = do
    tell l
    modify' $ \e -> e { pos = pos e + genericLength l }

file :: Parser ()
file = many (line >> eol) >> eof

line = do
    whitespace
    optional $ try assignment <|> try label <|> instruction

assignment = do
    x <- ident
    symbol "="
    v <- integer
    modify' $ \e -> e { vars = M.insert x v (vars e) }

label = do
    l <- ident
    symbol ":"
    modify' $ \e -> e { vars = M.insert l (pos e) (vars e) }

operand = Int <$> integer <|> Var <$> ident

instruction = do
    ins <- ident
    ops <- many operand
    emit $ take 3 $ Int (code M.! ins):ops ++ repeat (Int 0)

code = M.fromList
    [ ("lda", ram_out .|. a_in)
    , ("sta", a_out .|. ram_in)
    , ("ldb", ram_out .|. b_in)
    , ("stb", b_out .|. ram_in)
    , ("add", add_out .|. a_in)
    , ("sub", add_out .|. sub .|. a_in)
    , ("jmp", jump)
    , ("jz",  jump .|. zero)
    , ("jl",  jump .|. less)
    ]
    where {
        [ a_in
        , a_out
        , b_in
        , b_out
        , ram_in
        , ram_out
        , add_out
        , sub
        , jump
        , zero
        , less
        ] = bit <$> [0..10]
    }
