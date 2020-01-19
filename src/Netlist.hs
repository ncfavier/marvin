{-# LANGUAGE RecordWildCards #-}
module Netlist where

import Data.Bool
import Data.Bits
import Data.Char
import Data.Map (Map)
import qualified Data.Map as M
import Text.ParserCombinators.ReadP
import Text.Read hiding ((<++))

type Variable = String

newtype Value = Value [Bool]

instance Show Value where
    show (Value l) = bool '0' '1' <$> l

instance Read Value where
    readPrec = lift value

bitsToInteger :: [Bool] -> Integer
bitsToInteger = foldl f zeroBits
    where f n b = (n `shiftL` 1) .|. bool zeroBits (bit 0) b

bitsFromInteger :: Int -> Integer -> [Bool]
bitsFromInteger s n = [testBit n i | i <- [s - 1, s - 2..0]]

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
                       , vars      :: Map Variable Int
                       , equations :: Map Variable Expression
                       }

instance Read Netlist where
    readPrec = lift netlist

token p = skipSpaces >> p

comma = token (char ',')
colon = token (char ':')
equal = token (char '=')

identifier = token $ do
    c <- satisfy (\c -> isAlpha c || c == '_')
    s <- munch (\c -> isAlphaNum c || c == '_')
    return (c:s)

operator s = token (string s)

declaration = do
    x <- identifier
    size <- option 1 (colon >> integer)
    return (x, size)

value = token $ do
    l <- many1 $ (False <$ char '0' <++ char 'f') <++ (True <$ char '1' <++ char 't')
    return (Value l)

argument = (Avar <$> identifier) <++ (Aconst <$> value)

integer = token $ read <$> munch1 isDigit

expArg = Earg <$> argument

expReg = do
    "REG" <- identifier
    Ereg <$> identifier

expNot = do
    "NOT" <- identifier
    Enot <$> argument

expBinOp s c = do
    operator s
    c <$> argument <*> argument

expOr = expBinOp "OR" Eor
expXor = expBinOp "XOR" Exor
expAnd = expBinOp "AND" Eand
expNand = expBinOp "NAND" Enand

expMux = do
    "MUX" <- identifier
    Emux <$> argument <*> argument <*> argument

expConcat = do
    "CONCAT" <- identifier
    Econcat <$> argument <*> argument

expSlice = do
    "SLICE" <- identifier
    Eslice <$> integer <*> integer <*> argument

expSelect = do
    "SELECT" <- identifier
    Eselect <$> integer <*> argument

expRom = do
    "ROM" <- identifier
    Erom <$> integer <*> integer <*> argument

expRam = do
    "RAM" <- identifier
    Eram <$> integer <*> integer <*> argument <*> argument <*> argument <*> argument

expression = foldr1 (<++) [expReg, expNot, expOr, expXor, expAnd, expNand, expMux, expRom, expRam, expConcat, expSlice, expSelect, expArg]

equation = do
    x <- identifier
    equal
    exp <- expression
    return (x, exp)

netlist = do
    "INPUT" <- identifier
    invars <- identifier `sepBy` comma
    "OUTPUT" <- identifier
    outvars <- identifier `sepBy` comma
    "VAR" <- identifier
    vars <- M.fromList <$> declaration `sepBy` comma
    "IN" <- identifier
    equations <- M.fromList <$> many equation
    return Netlist{..}
