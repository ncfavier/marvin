{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
module Netlist where

import Control.Monad
import Control.Monad.State
import Data.Array.IArray
import Data.Bool
import Data.Char
import Data.Map (Map)
import qualified Data.Map as M
import Data.Void
import System.Exit
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Variable = Int

newtype Value = Value [Bool]

instance Show Value where
    show (Value l) = bool '0' '1' <$> l

instance Read Value where
    readsPrec _ s = case evalState (runParserT value "" s) initialEnvironment of
        Right v -> [(v, "")]
        _ -> []

data Argument = Avar Variable | Aconst Value

data Expression = Earg Argument
                | Ereg Variable
                | Enot Argument
                | Eor Argument Argument
                | Exor Argument Argument
                | Eand Argument Argument
                | Enand Argument Argument
                | Emux Argument Argument Argument
                | Econcat Argument Argument
                | Eslice Int Int Argument
                | Eselect Int Argument
                | Erom Int Int Argument
                | Eram Int Int Argument Argument Argument Argument

data Netlist = Netlist { invars    :: [Variable]
                       , outvars   :: [Variable]
                       , vars      :: Array Variable (String, Int)
                       , equations :: Array Variable Expression
                       }

data Environment = Environment { names :: Map String Variable }

initialEnvironment = Environment M.empty

type Parser = ParsecT Void String (State Environment)

readNetlist f = do
    input <- readFile f
    let r = flip evalState initialEnvironment $ runParserT netlist f input
    either (die . errorBundlePretty) return r

whitespace = L.space space1 empty empty
lineWhitespace = L.space (skipSome (oneOf " \t")) empty empty

lexeme = L.lexeme whitespace
lineLexeme = L.lexeme lineWhitespace
symbol = L.symbol whitespace
lineSymbol = L.symbol lineWhitespace

ident = try $ do
    c <- satisfy (\c -> isAlpha c || c == '_')
    s <- many $ satisfy (\c -> isAlphaNum c || c == '_')
    let i = c:s
    guard $ not $ i `elem` ["INPUT", "OUTPUT", "VAR", "IN"]
    return (c:s)

netlist :: Parser Netlist
netlist = do
    whitespace
    symbol "INPUT"
    invars' <- lexeme ident `sepBy` symbol ","
    symbol "OUTPUT"
    outvars' <- lexeme ident `sepBy` symbol ","
    symbol "VAR"
    varsAndSizes <- varAndSize `sepBy` symbol ","
    let nVars = length varsAndSizes
    let names = M.fromList [(x, i) | (i, (x, _)) <- zip [0..] varsAndSizes]
    modify' $ \e -> e { names }
    symbol "IN"
    equations <- array (0, nVars - 1) . concat <$> many (lineWhitespace >> option [] equation <* eol)
    eof
    let invars = map (names M.!) invars'
        outvars = map (names M.!) outvars'
        vars = listArray (0, nVars - 1) varsAndSizes
    return Netlist{..}

varAndSize = do
    x <- lexeme ident
    size <- option 1 (symbol ":" >> lexeme integer)
    return (x, size)

equation = do
    x <- variable
    lineSymbol "="
    exp <- expression
    return [(x, exp)]

value = do
    l <- some $ False <$ satisfy (\c -> c == '0' || c == 'f')
             <|> True <$ satisfy (\c -> c == '1' || c == 't')
    return (Value l)

variable = do
    v <- lineLexeme ident
    names <- gets names
    return (names M.! v)

argument = Avar <$> try variable <|> Aconst <$> lineLexeme value

integer = L.decimal

expArg = try $ Earg <$> argument

expReg = try $ do
    "REG" <- lineLexeme ident
    Ereg <$> variable

expNot = try $ do
    "NOT" <- lineLexeme ident
    Enot <$> argument

expBinOp s c = try $ do
    o <- lineLexeme ident
    guard (o == s)
    c <$> argument <*> argument

expOr = try $ expBinOp "OR" Eor
expXor = try $ expBinOp "XOR" Exor
expAnd = try $ expBinOp "AND" Eand
expNand = try $ expBinOp "NAND" Enand

expMux = try $ do
    "MUX" <- lineLexeme ident
    Emux <$> argument <*> argument <*> argument

expConcat = try $ do
    "CONCAT" <- lineLexeme ident
    Econcat <$> argument <*> argument

expSlice = try $ do
    "SLICE" <- lineLexeme ident
    Eslice <$> lineLexeme integer <*> lineLexeme integer <*> argument

expSelect = try $ do
    "SELECT" <- lineLexeme ident
    Eselect <$> lineLexeme integer <*> argument

expRom = try $ do
    "ROM" <- lineLexeme ident
    Erom <$> lineLexeme integer <*> lineLexeme integer <*> argument

expRam = try $ do
    "RAM" <- lineLexeme ident
    Eram <$> lineLexeme integer <*> lineLexeme integer <*> argument <*> argument <*> argument <*> argument

expression = choice [expReg, expNot, expOr, expXor, expAnd, expNand, expMux, expRom, expRam, expConcat, expSlice, expSelect, expArg]
