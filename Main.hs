module Main where

import "attoparsec" Data.Attoparsec.Text
import "base" Control.Applicative (many)
import "joint" Control.Joint ((<$$>), (<$$$>))
import "text" Data.Text (Text, pack)

import qualified "text" Data.Text.IO as T

data TT a = TT a [TT a] deriving Show

instance Functor TT where
	fmap f (TT x xs) = TT (f x) $ f <$$> xs

indented :: Int -> Parser (TT Text)
indented level = TT <$> subitem <*> many (indented $ level + 1) where

	subitem :: Parser Text
	subitem = count level (char '\t') *> takeTill (== '\n') <* (char '\n')

type Predicate = Text

type Object = Text

data Property = Property Predicate Object deriving Show

type Name = Text

data Item = Item Name [Property] deriving Show

item :: Parser Item
item = Item <$> takeTill (== ' ') <*> many (space *> property)

property :: Parser Property
property = Property <$> takeTill (== ':') <*> (char ':' *> takeTill (== ' '))

main :: IO ()
main = print =<< parseOnly item <$$$> parseOnly (indented 0) <$> T.readFile "resources/example.txt"
