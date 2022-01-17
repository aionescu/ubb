module Language.Xi.Parser(parse) where

import Data.Bifunctor(first)
import Data.Functor((<&>), ($>))
import Data.List(nub, foldl')
import Control.Monad.Except(liftEither, MonadError)
import Text.Parsec hiding (parse)

import Language.Xi.Syntax

type Expr' = Expr TyExpr
type Parser = Parsec String ()

reserved :: [String]
reserved = ["if", "and", "or", "let", "rec", "in", "ref", "print", "then", "else", "type", "as", "not"]

comma, colon, equals, shebang, multiLine, singleLine, comment, ws :: Parser ()
comma = ws <* char ',' <* ws
colon = ws <* char ':' <* ws
equals = ws <* char '=' <* ws
shebang = try $ string "#!" *> manyTill anyChar (endOfLine $> ()) $> ()
multiLine = try $ string "{-" *> manyTill (multiLine <|> (anyChar $> ())) (try $ string "-}") $> ()
singleLine = try $ string "--" *> manyTill anyChar (eof <|> endOfLine $> ()) $> ()
comment = singleLine <|> multiLine
ws = spaces *> skipMany (comment *> spaces)

parens :: Char -> Char -> Parser a -> Parser a
parens begin end = between (char begin *> ws) (char end *> ws)

tuple :: ([a] -> a) -> Parser a -> Parser a
tuple ctor term = parens '(' ')' $ uncurry mkTup <$> elems
  where
    withTrailing = (, True) <$> many (term <* comma)
    noTrailing = (, False) <$> sepBy1 term comma
    elems = try withTrailing <|> noTrailing

    mkTup [a] False = a
    mkTup l _ = ctor l

record :: Parser a -> Parser [(Ident, a)]
record rhs = unique =<< parens '{' '}' elems
  where
    withTrailing = many (term <* comma)
    noTrailing = sepBy1 term comma
    elems = try withTrailing <|> noTrailing
    term = (,) <$> (ident <* ws) <*> (rhs <* ws)

    unique es =
      let es' = fst <$> es
      in
        if nub es' == es'
          then pure es
          else fail "Fields in a record must be unique"

tyVar :: Parser TyExpr
tyVar = TyVar <$> tyIdent

tyTup :: Parser TyExpr
tyTup = tuple TyTup tyExpr

tyRec :: Parser TyExpr
tyRec = TyRec <$> record (colon *> ws *> tyExpr)

tyRef :: Parser TyExpr
tyRef = char '&' *> ws *> tyNoIntersect <&> TyRef

tyNoIntersect :: Parser TyExpr
tyNoIntersect = choice [try tyRef, try tyRec, try tyTup, tyVar]

tyNoFn :: Parser TyExpr
tyNoFn = chainr1 tyNoIntersect $ try $ ws *> char '&' *> ws $> TyIntersect

tyExpr :: Parser TyExpr
tyExpr = chainr1 tyNoFn $ try $ ws *> string "->" *> ws $> TyFn

number :: (Read a, Num a) => Parser a
number = read <$> many1 digit

intRaw :: Parser Integer
intRaw = sign <*> number
  where
    sign = option id (char '-' $> negate)

boolRaw :: Parser Bool
boolRaw = choice [string "True" $> True, string "False" $> False]

strRaw :: Parser String
strRaw = between quote quote $ many ch
  where
    unescape '\\' = '\\'
    unescape '"' = '"'
    unescape '0' = '\0'
    unescape 'n' = '\n'
    unescape 'r' = '\r'
    unescape 'v' = '\v'
    unescape 't' = '\t'
    unescape 'b' = '\b'
    unescape 'f' = '\f'
    unescape a = a

    escaped = char '\\' *> oneOf "\\\"0nrvtbf" <&> unescape
    regular = noneOf "\\\"\0\n\r\v\t\b\f"
    ch = regular <|> escaped
    quote = char '"'

int :: Parser Expr'
int = Lit . NumLit <$> intRaw

bool :: Parser Expr'
bool = Lit . BoolLit <$> boolRaw

str :: Parser Expr'
str = Lit . StrLit <$> strRaw

simpleLit :: Parser Expr'
simpleLit = choice [try str, try int, bool]

ident :: Parser String
ident = notReserved =<< (:) <$> fstChar <*> many sndChar
  where
    fstChar = lower
    sndChar = choice [letter, digit, char '\'']
    notReserved ((`elem` reserved) -> True) = fail "Reserved identifier"
    notReserved i = pure i

tyIdent :: Parser String
tyIdent = (:) <$> fstChar <*> many sndChar
  where
    fstChar = upper
    sndChar = choice [letter, digit, char '\'']

var :: Parser Expr'
var = Var <$> ident

member :: Parser Expr' -> Parser Expr'
member lhs = foldl' unroll <$> lhs <*> many (char '.' *> (Left <$> number <|> Right <$> ident) <* ws)
  where
    unroll lhs' (Left idx) = TupMember lhs' idx
    unroll lhs' (Right ident') = RecMember lhs' ident'

vTup :: Parser Expr'
vTup = tuple TupLit exprFull

vRecField :: Parser (Maybe Expr')
vRecField = optionMaybe $ unrollLam <$> (args <* ws) <*> (equals *> ws *> exprFull)

vRec :: Parser Expr'
vRec = RecLit . (punField <$>) <$> record vRecField
  where
    punField (i, Nothing) = (i, Var i)
    punField (i, Just e) = (i, e)

if' :: Parser Expr'
if' =
  If
  <$> (string "if" *> ws *> exprFull <* ws)
  <*> (string "then" *> ws *> exprNoSeq <* ws)
  <*> (string "else" *> ws *> exprNoSeq <* ws)

opMul :: Parser (Expr' -> Expr' -> Expr')
opMul =
  choice
  [ char '*' $> flip Arith Mul
  , char '/' $> flip Arith Div
  , char '%' $> flip Arith Rem
  ]
  <* ws

opAdd :: Parser (Expr' -> Expr' -> Expr')
opAdd =
  choice
  [ char '+' $> flip Arith Add
  , char '-' $> flip Arith Sub
  , char '&' $> Intersect
  ]
  <* ws

opComp :: Parser (Expr' -> Expr' -> Expr')
opComp =
  choice
  [ try $ string "<=" $> flip Comp LtEq
  , try $ string ">=" $> flip Comp GtEq
  , try $ string "==" $> flip Comp Eq
  , try $ string "!=" $> flip Comp NEq
  , char '<' $> flip Comp Lt
  , char '>' $> flip Comp Gt
  ]
  <* ws

opLogic :: Parser (Expr' -> Expr' -> Expr')
opLogic =
  choice
  [ string "and" $> flip Logic And
  , string "or" $> flip Logic Or
  ]
  <* ws

type Arg = Maybe (Ident, TyExpr)

arg :: Parser Arg
arg = try $ parens '(' ')' $ optionMaybe $ (,) <$> (ident <* colon) <*> tyExpr

args :: Parser [Arg]
args = many $ arg <* ws

unrollLam :: [Arg] -> Expr' -> Expr'
unrollLam [] e = e
unrollLam (Nothing : as) e = Lam "_" tyUnit $ unrollLam as e
unrollLam (Just (i, t) : as) e = Lam i t $ unrollLam as e

lam :: Parser Expr'
lam = unrollLam <$> (many1 arg <* string "->" <* ws) <*> exprNoSeq

deref :: Parser Expr'
deref = char '!' *> ws *> exprNoOps <&> Deref

exprNoMember :: Parser Expr'
exprNoMember = choice (try <$> [not', ref, print', if', deref, lam, vRec, vTup, simpleLit, var]) <* ws

exprNoOps :: Parser Expr'
exprNoOps = try (member exprNoMember) <|> exprNoMember

exprApp :: Parser Expr'
exprApp = chainl1 exprNoOps (ws $> App)

as' :: Parser Expr'
as' = As <$> (exprApp <* ws <* string "as") <*> (ws *> tyNoIntersect)

exprAs :: Parser Expr'
exprAs = try as' <|> exprApp

exprMul :: Parser Expr'
exprMul = chainl1 exprAs opMul

exprAdd :: Parser Expr'
exprAdd = chainl1 exprMul opAdd

exprComp :: Parser Expr'
exprComp = chainl1 exprAdd opComp

exprLogic :: Parser Expr'
exprLogic = chainr1 exprComp opLogic

expr :: Parser Expr'
expr = exprLogic

print' :: Parser Expr'
print' = Print <$> (string "print" <* ws *> expr)

ref :: Parser Expr'
ref = string "ref" *> ws *> expr <&> RefExpr

not' :: Parser Expr'
not' = string "not" *> ws *> exprNoOps <&> Not

assign :: Parser Expr'
assign = Assign <$> expr <*> (string ":=" *> ws *> expr)

unrollTypes :: [Arg] -> TyExpr -> TyExpr
unrollTypes [] t = t
unrollTypes (Nothing : ts) t = TyFn tyUnit $ unrollTypes ts t
unrollTypes (Just (_, a) : ts) t = TyFn a $ unrollTypes ts t

unrollLet :: Bool -> Ident -> [Arg] -> Maybe TyExpr -> Expr' -> Expr' -> Expr'
unrollLet r i as t v = Let r i (unrollTypes as <$> t) (unrollLam as v)

let' :: Parser Expr'
let' =
  unrollLet
  <$> (string "let" *> ws *> option False (try $ string "rec" $> True))
  <*> (ws *> ident)
  <*> (ws *> args)
  <*> optionMaybe (try $ colon *> tyExpr)
  <*> (equals *> exprFull)
  <*> (ws *> string "in" *> ws *> exprFull)

letTy :: Parser Expr'
letTy = LetTy <$> (string "type" *> ws *> tyIdent) <*> (equals *> tyExpr) <*> (ws *> string "in" *> ws *> exprFull)

exprNoSeq :: Parser Expr'
exprNoSeq = choice [try letTy, try let', try assign, expr]

exprFull :: Parser Expr'
exprFull = exprNoSeq `chainr1` (char ';' *> ws $> Seq)

program :: Parser Expr'
program = option () shebang *> ws *> exprFull <* eof

parse :: MonadError String m => String -> m Expr'
parse = liftEither . first show . runParser program () ""
