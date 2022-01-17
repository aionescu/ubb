module FA.Parser(parseFA) where

import Control.Monad((>=>))
import Data.Bifunctor(first)
import Data.Containers.ListUtils(nubOrdOn)
import Data.Functor(void, (<&>))
import Data.Map.Strict(Map)
import Data.Map.Strict qualified as M
import Data.Maybe(mapMaybe)
import Data.Set(Set)
import Data.Set qualified as S
import Text.Parsec hiding (State, parse)

import FA.Core

data Stmt
  = Transition State [Char] State
  | Initial State
  | Final State
  deriving stock Show

getInitial :: [Stmt] -> Either String State
getInitial stmts =
  case mapMaybe toInitial stmts of
    [s] -> Right s
    [] -> Left "No initial state found"
    _ -> Left "Multiple initial states found"
  where
    toInitial (Initial s) = Just s
    toInitial _ = Nothing

getFinal :: [Stmt] -> Either String (Set State)
getFinal stmts =
  case mapMaybe toFinal stmts of
    [] -> Left "No final state found"
    fs -> Right $ S.fromList fs
  where
    toFinal (Final s) = Just s
    toFinal _ = Nothing

getTransitions :: [Stmt] -> Either String (Map (State, Char) [State])
getTransitions stmts
  | length uniq /= length transitions = Left "Duplicate transitions were found"
  | otherwise = Right ts
  where
    ts = M.fromListWith (<>) tuples
    tuples = toTuples =<< transitions
    toTuples (a, cs, b) = (\c -> ((a, c), [b])) <$> cs

    uniq = nubOrdOn (\(a, _, b) -> (a, b)) transitions
    transitions = mapMaybe toTransition stmts

    toTransition (Transition a cs b) = Just (a, cs, b)
    toTransition _ = Nothing

getStates :: [Stmt] -> Set State
getStates = foldMap toStates
  where
    toStates (Initial s) = [s]
    toStates (Final s) = [s]
    toStates (Transition a _ b) = [a, b]

getAlphabet :: [Stmt] -> Set Char
getAlphabet = foldMap toChars
  where
    toChars (Transition _ cs _) = S.fromList cs
    toChars _ = []

buildFA :: [Stmt] -> Either String FA
buildFA stmts =
  first ("Invalid FA: " <>) $
    FA (getStates stmts)
    <$> getInitial stmts
    <*> getFinal stmts
    <*> pure (getAlphabet stmts)
    <*> getTransitions stmts

type Parser = Parsec String ()

comment :: Parser ()
comment = void $ try $ string "--" *> manyTill anyChar (eof <|> void endOfLine)

ws :: Parser ()
ws = spaces *> skipMany (comment *> spaces)

ident :: Parser String
ident = (:) <$> fstChar <*> many sndChar
  where
    fstChar = choice [letter, char '_', char '\'']
    sndChar = fstChar <|> digit

state :: Parser State
state = ident <* ws

initial :: Parser Stmt
initial = string "->" *> ws *> state <&> Initial

final :: Parser Stmt
final = state <* string "->" <* ws <&> Final

many1Till :: Parser a -> Parser end -> Parser [a]
many1Till a end = manyTill a end >>= nonEmpty
  where
    nonEmpty [] = fail "Expected at least 1 item in many1Till"
    nonEmpty l = pure l

transition' :: Parser [Char]
transition' = string "-[" *> many1Till anyChar (char ']') <* string "->" <* ws

transition :: Parser Stmt
transition = Transition <$> state <*> transition' <*> state

stmt :: Parser Stmt
stmt = choice [try initial, try final, try transition]

fa :: Parser [Stmt]
fa = many1 stmt

parse :: Parser a -> String -> Either String a
parse p = first (("Parser error:\n" <>) . show) . runParser p () ""

parseFA :: String -> Either String FA
parseFA = parse (ws *> fa <* eof) >=> buildFA
