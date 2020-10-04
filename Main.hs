module Main where

import "parsec" Text.Parsec (ParsecT, try, many, parse, noneOf, char, count)

data TT = TT String [TT] deriving Show

indented :: Monad m => Int -> ParsecT String u m TT
indented level = TT <$> try item <*> many (indented $ level + 1) where

	item :: Monad m => ParsecT String u m String
	item = count level (char '\t') *> many (noneOf "\t\n") <* char '\n'

main :: IO ()
main = readFile "resources/example.txt" >>= print . parse (indented 0) ""
