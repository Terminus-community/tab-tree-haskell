module Main where

import "attoparsec" Data.Attoparsec.Text
import "base" Control.Applicative (many)
import "text" Data.Text (Text)

import qualified "text" Data.Text.IO as T

data TT a = TT a [TT a] deriving Show

indented :: Int -> Parser (TT Text)
indented level = TT <$> subitem <*> many (indented $ level + 1) where

	subitem :: Parser Text
	subitem = count level (char '\t') *> takeTill (== '\n') <* (char '\n')

main :: IO ()
main = T.readFile "resources/example.txt" >>= print . parseOnly (indented 0)
