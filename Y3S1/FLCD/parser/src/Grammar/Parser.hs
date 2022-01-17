module Grammar.Parser where

import Data.Functor((<&>), ($>))
import Data.Map qualified as M
import Text.Parsec hiding (parse)
import Grammar.Core

type Parser = Parsec String ()

escaped :: Parser Char
escaped = regular <|> unescaped
  where
    unescape '\\' = '\\'
    unescape '0' = '\0'
    unescape 'n' = '\n'
    unescape 'r' = '\r'
    unescape 'v' = '\v'
    unescape 't' = '\t'
    unescape 'b' = '\b'
    unescape 'f' = '\f'
    unescape a = a

    unescaped = char '\\' *> oneOf "\"\\0nrvtbf" <&> unescape
    regular = noneOf "\"\\\0\n\r\v\t\b\f"

terminal :: Parser Atom
terminal = between (char '"') (char '"') $ Terminal <$> many1 escaped

nonTerminalRaw :: Parser String
nonTerminalRaw = (:) <$> fstChar <*> many sndChar
  where
    fstChar = letter <|> char '_'
    sndChar = fstChar <|> digit

nonTerminal :: Parser Atom
nonTerminal = NonTerminal <$> nonTerminalRaw

builtin :: Parser Atom
builtin = choice [string "ID" $> BuiltinId, string "CONST" $> BuiltinConst]

atom :: Parser Atom
atom = (builtin <|> terminal <|> nonTerminal) <* spaces

eps :: Parser Prod
eps = char 'ϵ' <* spaces $> Eps

prod :: Parser Prod
prod = eps <|> Prod <$> many1 atom

rhs' :: Parser [Prod]
rhs' = prod `sepBy1` (char '|' *> spaces)

line :: Parser (Prod, [Prod])
line = (,) <$> (prod <* string "->" <* spaces) <*> (rhs' <* char ';' <* spaces)

grammar :: Parser (Grammar Prod)
grammar = Grammar <$> (M.fromListWith (<>) <$> many1 (try line)) <*> nonTerminalRaw <* spaces

full :: Parser a -> Parser a
full p = spaces *> p <* eof

parse :: Parser a -> String -> Either ParseError a
parse p = runParser p () ""
