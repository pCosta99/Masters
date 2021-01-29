module Parsing where

import Data.Char
import Libs.Cp
import Control.Monad hiding (ap, join)
import Control.Applicative hiding ((<|>))

infix 1 <|>

-- A parser consumes a list of a type of input and creates a interpreted value paired with the unread input.
newtype Parser s r = Parser { parse :: [s] -> [(r,[s])] }

-- Checks if a Parser of strings starts with a certain char.
symbol :: Char -> Parser Char Char
symbol s = Parser $ (\l -> if length l == 0 || s /= head l then [] else [(s, tail l)])

-- A more generic symbol function.
satisfy :: (a -> Bool) -> Parser a a
satisfy p = Parser $ (\l -> if length l == 0 || not (p (head l)) then [] else [(head l, tail l)])

-- Checks the parser for a starting token.
token :: Eq a => [a] -> Parser a [a]
token t = Parser $ (\inp -> if take (length t) inp == t then [(t, drop (length t) inp)] else [])

-- Semigroup instance allows us to combine parsers with a or logic style.
instance Semigroup (Parser s r) where
    (Parser p) <> (Parser q) = Parser $ \inp -> p inp ++ q inp

-- Small fix to force semigroup to be the most "powerful" infix.
-- Had to force the signature for some reason.
(<|>) :: Parser s r -> Parser s r -> Parser s r
(<|>) = (<>)

-- The functor instance allows us to convert the results of one parsing application into something of a different type.
instance Functor (Parser s) where
    --fmap f (Parser p) = Parser $ \inp -> [(f r, inp') | (r,inp') <- p inp]
    fmap f (Parser p) = Parser $ \inp -> map (f >< id) $ p inp

-- Allows us to sequencially combine parsers.
instance Applicative (Parser s) where
    pure a = Parser $ \inp -> [(a, inp)] -- not entirely sure when to use pure
    --(Parser p) <*> (Parser q) = Parser $ \inp -> [(f r, ys) | (f, xs) <- p inp, (r, ys) <- q xs]
    (Parser p) <*> (Parser q) = Parser $ \inp -> map (id >< q) >=> (\(x,xs) -> map (x >< id) xs) $ p inp

-- No clue what power this gives us but should give some.
instance Monad (Parser s) where
    return = pure
    (>>=) = flip $ bind id

-- "Downgrades" the parser.
join :: Parser s (Parser s r) -> Parser s r
join (Parser p) = Parser $ concat . map (ap . (parse >< id)) . p

-- Left to right kleisli composition.
bind :: (a -> Parser s b) -> (b -> Parser s c) -> a -> Parser s c
bind f g = join . fmap g . f

-- Repeteadly uses a parser.
infiniteP :: Parser s r -> Parser s [r] -> Parser s [r]
infiniteP p f = (:) <$> p <*> f

-- Parses a pattern 0 or more times.
zeroOrMore :: Parser Char Char -> Parser Char [Char]
zeroOrMore p = infiniteP p (oneOrMore p) <|> pure []

-- Parses a pattern 1 or more times.
oneOrMore :: Parser Char Char -> Parser Char [Char]
oneOrMore p = infiniteP p (oneOrMore p) <|> singl <$> p

-- Parses a pattern n or more times.
nOrMore :: Int -> Parser Char Char -> Parser Char [Char]
nOrMore n p = infiniteP p (nOrMore n p) <|> replicateM n p

-- Parses 0 or more spaces.
spaces' :: Parser Char [Char]
spaces' = zeroOrMore $ satisfy isSpace

-- Parses a token followed by spaces.
token' :: [Char] -> Parser Char [Char]
token' t = curry fst <$> token t <*> spaces'

-- Parses a symbol followed by spaces.
symbol' :: Char -> Parser Char Char
symbol' t = curry fst <$> symbol t <*> spaces'

-- Parses a list of items separated by something.
separatedBy :: Parser s a -> Parser s b -> Parser s [a]
separatedBy p s = singl <$> p <|> g <$> p <*> s <*> separatedBy p s where g a b c = a:c

-- Parses something enclosed by two parsers.
enclosedBy :: Parser s a -> Parser s b -> Parser s c -> Parser s b
enclosedBy = liftA3 (\a b c -> b)
