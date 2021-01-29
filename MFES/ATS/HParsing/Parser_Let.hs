module Parser_Let where

import Parsing
import Data.Char
import Prelude hiding (exp)

data Let = Let Items Exp deriving Show

type Items = [Item]

data Item = NestedLet String Let | Atrib String Exp deriving Show

type Exp = String

plet = f <$> token' "let" <*> enclosedBy (symbol' '{') items (symbol' '}') <*> token' "in" <*> exp where
    f a b c d = Let b d

items = separatedBy item (symbol' ';')

item = f <$> myid <*> symbol' '=' <*> exp  <|> g <$> myid <*> symbol' '=' <*> plet where
        f a b c = Atrib a c
        g a b c = NestedLet a c

exp = myid <|> pNat

myid = curry fst <$> oneOrMore (satisfy isAlpha) <*> spaces'

pNat = curry fst <$> oneOrMore (satisfy isDigit) <*> spaces'
