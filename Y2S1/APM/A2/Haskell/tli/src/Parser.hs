module Parser(parse) where

import Data.Functor(($>))
import Control.Applicative(liftA2)
import Text.Parsec hiding (parse)

import AST

type Parser = Parsec String ()

multiLine :: Parser ()
multiLine = try $ string "{-" *> manyTill (multiLine <|> (anyChar $> ())) (try $ string "-}") $> ()

singleLine :: Parser ()
singleLine = try $ string "--" *> manyTill anyChar newline $> ()

comment :: Parser ()
comment = singleLine <|> multiLine

ws :: Parser ()
ws = spaces *> skipMany (comment *> spaces)

int :: Parser Val
int = VInt <$> (sign <*> number)
  where
    sign = option id (char '-' $> negate)
    number = read <$> many1 digit

bool :: Parser Val
bool = VBool <$> choice [string "True" $> True, string "False" $> False]

val :: Parser Val
val = int <|> bool

ident :: Parser String
ident = liftA2 (:) fstChar (many sndChar)
  where
    fstChar = choice [letter, char '_']
    sndChar = choice [fstChar, digit, char '\'']

type' :: Parser Type
type' = choice [string "Int" $> TInt, string "Bool" $> TBool]

parens :: Parser a -> Parser a
parens = between (char '(' <* ws) (char ')' <* ws)

opMul :: Parser (Expr -> Expr -> Expr)
opMul =
  choice
  [ char '*' $> flip Arith Multiply
  , char '/' $> flip Arith Divide
  , char '%' $> flip Arith Remainder
  ]
  <* ws

opAdd :: Parser (Expr -> Expr -> Expr)
opAdd =
  choice
  [ char '+' $> flip Arith Add
  , char '-' $> flip Arith Subtract
  ]
  <* ws

opComp :: Parser (Expr -> Expr -> Expr)
opComp =
  choice
  [ try $ string "<=" $> flip Comp LtEq
  , try $ string ">=" $> flip Comp GtEq
  , try $ string "<>" $> flip Comp NEq
  , char '<' $> flip Comp Lt
  , char '>' $> flip Comp Gt
  , char '=' $> flip Comp Eq
  ]
  <* ws

opLogic :: Parser (Expr -> Expr -> Expr)
opLogic =
  choice
  [ string "and" $> flip Logic And
  , string "or" $> flip Logic Or
  ]
  <* ws

termMul :: Parser Expr
termMul = choice [try $ parens expr, Lit <$> try val, Var <$> ident] <* ws

termAdd :: Parser Expr
termAdd = chainl1 termMul opMul

termComp :: Parser Expr
termComp = chainl1 termAdd opAdd

termLogic :: Parser Expr
termLogic = chainl1 termComp opComp

expr :: Parser Expr
expr = chainl1 termLogic opLogic

print' :: Parser Stmt
print' = Print <$> (string "print" <* ws *> expr)

decl :: Parser Stmt
decl = liftA2 Decl (ident <* colon) type'
  where
    colon = ws <* char ':' <* ws

assign :: Parser Stmt
assign = liftA2 Assign (ident <* arrow) expr
  where
    arrow = ws *> string "<-" *> ws

block :: Parser Stmt
block =
  char '{' *> ws
  *> stmt
  <* ws <* char '}' <* ws

if' :: Parser Stmt
if' = If <$> cond <*> block <*> option Nop elseBlock
  where
    cond = string "if" *> ws *> expr <* ws
    elseBlock = string "else" *> ws *> block

while :: Parser Stmt
while = While <$> cond <*> block
  where
    cond = string "while" *> ws *> expr <* ws

stmt' :: Parser Stmt
stmt' = choice [try while, try if', try assign, try decl, print'] <* ws

stmt :: Parser Stmt
stmt = option Nop $ stmt' `chainr1` (char ';' *> ws $> Compound)

program :: Parser Program
program = ws *> stmt <* eof

parse :: String -> TLI Program
parse = toTLI . runParser program () ""
