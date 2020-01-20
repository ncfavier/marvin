{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
module Assembler where

import Control.Monad.State
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

data Environment = Environment { vars :: Map String Integer
                               , pos  :: Integer
                               }

initialEnvironment = Environment M.empty 0

data V = Int Integer | Var String
data W = Ins Integer | Imm V | Mem V | Reg String

type Parser = ParsecT Void String (State Environment)

wordSize = 42

usage = do
    progName <- getProgName
    die $ "usage: " ++ progName ++ " FILE"

main = do
    args <- getArgs
    f <- maybe usage pure case args of
        [f] -> Just f
        _ -> Nothing
    input <- readFile f
    let (r, Environment { vars }) = flip runState initialEnvironment
                                  $ runParserT file f
                                  $ input
    output <- either (die . errorBundlePretty) return r
    print wordSize -- word size
    print 2048 -- ram size
    let makeWord (Ins i) = i
        makeWord (Imm v) = getValue v `shiftL` 2
        makeWord (Mem v) = getValue v `shiftL` 2 .|. bit 0
        makeWord (Reg "a") = bit 2 .|. bit 1
        makeWord (Reg "b") = bit 3 .|. bit 1
        makeWord (Reg r) = error $ "invalid register " ++ r
        getValue (Int i) = i
        getValue (Var v) = vars M.! v
    mapM_ (print . makeWord) output

whitespace = L.space (skipSome (oneOf " \t")) (L.skipLineComment ";") empty

lexeme = L.lexeme whitespace
symbol = L.symbol whitespace

integer = lexeme L.decimal

ident = lexeme $ some $ satisfy (\c -> isLetter c || c == '_')

emit l = l <$ modify' (\e -> e { pos = pos e + genericLength l })

file :: Parser [W]
file = concat <$> many (line <* eol) <* eof

line = do
    whitespace
    option [] $ try assignment <|> try label <|> instruction

assignment = do
    x <- ident
    symbol "="
    v <- integer
    modify' $ \e -> e { vars = M.insert x v (vars e) }
    return []

label = do
    l <- ident
    symbol ":"
    modify' $ \e -> e { vars = M.insert l (pos e) (vars e) }
    return []

instruction = do
    ins <- ident
    ops <- many operand
    let ops' | ins `elem` ["inc", "dec"] = Ins 0:ops
             | otherwise = ops
    emit $ take 3 $ Ins (code M.! ins):ops' ++ repeat (Ins 0)

operand = Reg <$> register <|> Mem <$> mem <|> Imm <$> value

register = char '%' >> ident

mem = char '*' >> value

value = Int <$> integer <|> Var <$> ident

code = M.fromList
    [ ("mov", mov)
    , ("add", mov .|. addsub)
    , ("sub", mov .|. addsub .|. sub)
    , ("inc", mov .|. inc)
    , ("dec", mov .|. dec)
    , ("mul", mov .|. mul)
    , ("div", mov .|. divmod)
    , ("mod", mov .|. divmod .|. mod)
    , ("jmp", jump)
    , ("jz",  jump .|. zero)
    , ("jl",  jump .|. less)
    ]
    where {
        [ mov
        , addsub
        , sub
        , inc
        , dec
        , mul
        , divmod
        , mod
        , jump
        , zero
        , less
        ] = bit <$> [0..10]
    }
