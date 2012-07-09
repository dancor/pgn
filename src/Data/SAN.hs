module Data.SAN (
  Move(..),
  Annotation,
  Piece,
  moveAnnParser,
  showMove
) where

import Control.Applicative hiding (many, (<|>))
import Control.Monad
import Data.Char
import Text.Parsec

data Move = Move {
  mvPiece :: Maybe Piece,
  mvFromX :: Maybe Int,
  mvFromY :: Maybe Int,
  mvIsACapture :: Bool,
  mvToX :: Int,
  mvToY :: Int,
  mvPromote :: Maybe Piece
  } |
  Castle Piece
  deriving (Show, Ord, Eq)

type Annotation = String

type Piece = Char

posIntParser :: Parsec String () Int
posIntParser = read <$> many1 digit

castlingParser :: Parsec String () Move
castlingParser = string "O-O" >>
  option (Castle 'K') (string "-O" >> return (Castle 'Q'))

annotationParser :: Parsec String () Annotation
annotationParser = many $ oneOf "!?+#"

colParser :: Parsec String () Int
colParser = (\ x -> ord x - ord 'a' + 1) <$> oneOf ['a'..'w']

moveParser :: Parsec String () Move
moveParser = castlingParser <|> do
  pieceMb <- optionMaybe upper
  col1Mb <- optionMaybe colParser
  row1Mb <- optionMaybe posIntParser
  case (pieceMb, col1Mb) of
    (Nothing, Nothing) -> fail "missing starting piece or column"
    _ -> return ()
  isCap <- option False $ char 'x' >> return True
  col2Mb <- optionMaybe colParser
  row2Mb <- optionMaybe posIntParser
  (col1Mb', row1Mb', col2, row2) <-
    case (isCap, col1Mb, row1Mb, col2Mb, row2Mb) of
      (True, _, _, Nothing, _) -> fail "missing ending column for capture"
      (True, _, _, _, Nothing) -> fail "missing ending row for capture"
      (False, Nothing, Nothing, Nothing, _) -> fail "missing starting column"
      (False, Nothing, Nothing, _, Nothing) -> fail "missing starting row"
      (False, Just col1, Just row1, Nothing, Nothing) ->
        return (Nothing, Nothing, col1, row1)
      (_, col1Mb, row1Mb, Just col2, Just row2) ->
        return (col1Mb, row1Mb, col2, row2)
      _ -> fail "impossible"
  promote <- optionMaybe $ char '=' >> upper
  ann <- optionMaybe annotationParser
  return $ Move pieceMb col1Mb' row1Mb' isCap col2 row2 promote

showMove (Move p x y isCap x2 y2 prom) = concat [
  maybe "" (:[]) p,
  maybe "" showCol x,
  maybe "" show y,
  if isCap then "x" else "",
  showCol x2,
  show y2,
  maybe "" (('=' :) . (:[])) prom
  ]
  where
  showCol x = [chr $ x + ord 'a' - 1]
showMove (Castle p) = if p == 'K' then "O-O" else "O-O-O"

moveAnnParser :: Parsec String () (Move, Annotation)
moveAnnParser = liftM2 (,) moveParser annotationParser
