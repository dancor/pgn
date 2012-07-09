{-# LANGUAGE FlexibleContexts #-}

import Control.Applicative
import Data.Either.Unwrap
import Text.Parsec as P

import Data.PGN
import Data.SAN

eol :: Stream s m Char => P.ParsecT s u m Char
eol = P.char '\n'

restOfLine :: Stream s m Char => P.ParsecT s u m String
restOfLine = P.many1 (P.noneOf "\n") <* eol

--spaces = P.many P.space

parseFics = do
  p1 <- P.many1 (P.noneOf " ")
  string " ("
  r1 <- P.many1 P.digit
  string ") vs. "
  p2 <- P.many1 (P.noneOf " ")
  string " ("
  r2 <- P.many1 P.digit
  P.string ") --- "
  date <- restOfLine
  description <- restOfLine
  eol
  string "Move  " >> restOfLine
  string "----  " >> restOfLine
  let crapBtwnMoves = P.many (P.oneOf " ():.0123456789\n")
  crapBtwnMoves
  moves <- P.many (moveAnnParser <* crapBtwnMoves)
  P.many (P.oneOf "{} " P.<|> P.letter)
  Left result <- resultOrMoveParser
  return $ PGN [
    ("White", p1),
    ("Black", p2),
    ("Event", description)] moves result

main = do
  f <- readFile "test"
  putStrLn . showPgn . fromRight $ runParser parseFics () "test" f
