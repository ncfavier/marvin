{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
module Netlist where

import Control.Monad.Writer hiding (lift)
import Control.Monad.State hiding (lift)
import Data.Bool
import Data.Char
import Data.Word
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Text.ParserCombinators.ReadP hiding (get)
import Text.Read hiding ((<++), get)

type Variable = String

newtype Value = Value [Bool]

instance Show Value where
    show (Value l) = bool '0' '1' <$> l

instance Read Value where
    readPrec = lift value

data Argument = Avar Variable | Aconst Value
              deriving Show

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
                deriving Show

data Netlist = Netlist { invars    :: [Variable]
                       , outvars   :: [Variable]
                       , vars      :: [(Variable, Int)]
                       , equations :: Map Variable Expression
                       } deriving Show

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
    inv <- identifier `sepBy` comma
    "OUTPUT" <- identifier
    outv <- identifier `sepBy` comma
    "VAR" <- identifier
    v <- declaration `sepBy` comma
    "IN" <- identifier
    eqs <- many equation
    return (Netlist inv outv v (M.fromList eqs))

-- TODO: remove
topsort eqs = execWriter $ flip execStateT S.empty $ mapM_ addEq (M.keys eqs)
    where
        addEq = go S.empty where
            go seen x = do
                env <- get
                when (x `S.notMember` env) do
                    when (x `S.member` seen) $ error "cycle"
                    let exp = eqs M.! x
                        arg (Avar y) = go (S.insert x seen) y
                        arg (Aconst _) = return ()
                    case exp of
                        Earg a              -> arg a
                        Enot a              -> arg a
                        Eor a b             -> arg a >> arg b
                        Exor a b            -> arg a >> arg b
                        Eand a b            -> arg a >> arg b
                        Enand a b           -> arg a >> arg b
                        Emux s a b          -> arg s >> arg a >> arg b
                        Econcat a b         -> arg a >> arg b
                        Eslice i j a        -> arg a
                        Eselect i a         -> arg a
                        Erom a s ra         -> arg ra
                        Eram a s ra we wa w -> arg ra >> arg we >> arg wa >> arg w
                        _                   -> return ()
                    tell [(x, exp)]
                    modify' $ S.insert x
