module Data.PGN (
  PGN(..),
  Result(..),
  pgnParser,
  showPgn,
  -- resultOrMoveParser should only be exposed in an internals-only module?
  resultOrMoveParser
) where

import Control.Applicative hiding (many, optional, (<|>))
import Control.Arrow
import Control.Monad
import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import Data.SAN
import Text.Parsec as P
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as P

-- TODO: separated annotations like "1. d4 $9" as well
data PGN = PGN {
  pgnTags :: [Tag],
  pgnMoves :: [(Move, Annotation)],
  pgnResult :: Result
  }
  deriving (Eq, Show)

type Tag = (String, String)

data Result = WhiteWin | BlackWin | DrawnGame | OtherResult
  deriving (Eq, Show, Enum, Ord)

lexer :: P.TokenParser ()
lexer = P.makeTokenParser emptyDef {
  P.commentStart = "{",
  P.commentEnd   = "}",
  P.commentLine  = ";",
  P.nestedComments = False
  }

-- Bind the lexical parsers at top-level.
whiteSpace :: Parsec String () ()
whiteSpace = P.whiteSpace lexer

decimal :: Parsec String () Integer
decimal = P.decimal lexer

lexeme :: Parsec String () a -> Parsec String () a
lexeme = P.lexeme lexer

-- Parsec String () for a PGN symbol token.
pgnSymbol :: Parsec String () String
pgnSymbol = lexeme $ do
  first <- alphaNum
  rest <- many (alphaNum <|> P.oneOf "_+#=:-")
  return (first:rest)

-- Parsec String () for a PGN string token.
pgnString :: Parsec String () String
pgnString = lexeme $ between (char '"') (char '"') (many pgnStringChar)

-- Parsec String () for characters inside a string token.
pgnStringChar :: Parsec String () Char
pgnStringChar = do
  c <- satisfy (\c -> isPrint c && c /= '"')
  if c == '\\'
    then P.oneOf "\\\""
    else return c

pgnTagName :: Parsec String () String
pgnTagName = lexeme . liftM2 (:) alphaNum . many $ alphaNum <|> char '_'

pgnInteger :: Parsec String () Integer
pgnInteger = lexeme decimal

tagParser :: Parsec String () Tag
tagParser = liftM2 (,)
  (lexeme (char '[') >> pgnTagName)
  (pgnString <* lexeme (char ']'))

-- We should check the move numbers?  But what is best way to deal with
-- misnumbered input anyway?
resultOrMoveParser :: Parsec String () (Either Result (Move, Annotation))
resultOrMoveParser = (string "*" >> return (Left OtherResult))
  <|> do
    num <- pgnInteger
    case num of
      0 -> (string "-1" >> return (Left BlackWin)) <|> moveAfterNum (Just num)
      1 -> (string "-0" >> return (Left WhiteWin)) <|>
        (string "/2-1/2" >> return (Left DrawnGame)) <|>
        moveAfterNum (Just num)
      _ -> moveAfterNum $ Just num
  <|> (moveAfterNum =<< optionMaybe pgnInteger)
  where
  moveAfterNum (Just n) = lexeme (skipMany (char '.')) >> moveAfterNum Nothing
  moveAfterNum Nothing  = Right <$> lexeme moveAnnParser

showResult BlackWin = "0-1"
showResult WhiteWin = "1-0"
showResult DrawnGame = "1/2-1/2"

stopOnLeft :: (Monad m) => m (Either d a) -> m (d, [a])
stopOnLeft p = p >>= \ res -> case res of
  Left stopRes -> return (stopRes, [])
  Right rRes -> second (rRes:) `liftM` stopOnLeft p

pgnParser :: Parsec String () PGN
pgnParser = whiteSpace >> do
  tags <- many tagParser
  (result, moves) <- stopOnLeft resultOrMoveParser <* whiteSpace
  return $ PGN tags moves result

showPgn (PGN tags moves result) = 
  concatMap (\ (t, v) -> "[" ++ t ++ " \"" ++ v ++ "\"]\n") 
    (tags ++ [("Result", resStr)]) ++
  -- todo: annotations
  intercalate " "
    (concat . zipWith (\ n l -> (show n ++ ".") : l) [1..] . splitEvery 2 $ 
    map (showMove . fst) moves) ++
  " " ++ resStr
  where
  resStr = showResult result
