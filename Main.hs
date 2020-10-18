module Main where

import "attoparsec" Data.Attoparsec.Text
import "base" Control.Applicative (many, (<|>))
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

data Object = Description Text | Code Text deriving Show

object :: Parser Object
object = code <|> description where

	code = Code <$> (char '`' *> takeTill (== '`') <* char '`')
	description = Description <$> takeTill (== ' ')

data Property = Property Predicate Object deriving Show

type Name = Text

data Item = Item Name [Property] deriving Show

item :: Parser Item
item = Item <$> takeTill (== ' ') <*> many (space *> property)

property :: Parser Property
property = Property <$> takeTill (== ':') <*> (char ':' *> object)

main :: IO ()
main = print =<< parseOnly item <$$$> parseOnly (indented 0)
	<$> T.readFile "resources/russia_elx.tree.txt"
