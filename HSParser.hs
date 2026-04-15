module HSParser where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad (MonadPlus (..), void)
import qualified Data.Bifunctor as Bifunctor
import Data.Char (isDigit, isLower)
import HSTipus

-- Parser
-- https://deepsource.com/blog/monadic-parser-combinators

newtype Parser a = Parser {parse :: String -> [(a, String)]}

instance Functor Parser where
  fmap f p = Parser (fmap (Bifunctor.first f) . parse p)

instance Applicative Parser where
  pure = result
  p1 <*> p2 = Parser $ \inp -> do
    (f, inp') <- parse p1 inp
    (a, inp'') <- parse p2 inp'
    return (f a, inp'')

instance Monad Parser where
  -- a -> Parser a
  return = pure
  -- Parser a -> (a -> Parser b) -> Parser b
  p >>= f = Parser $ \inp ->
    concat [parse (f v) inp' | (v, inp') <- parse p inp]

plus :: Parser a -> Parser a -> Parser a
p `plus` q = Parser $ \inp -> parse p inp ++ parse q inp

or' :: Parser a -> Parser a -> Parser a
p `or'` q = Parser $ \inp -> case parse (p `plus` q) inp of
  [] -> []
  (x : xs) -> [x]

instance MonadPlus Parser where
  mzero = zero
  mplus = plus

instance Alternative Parser where
  empty = zero
  (<|>) = or'

result :: a -> Parser a
result val = Parser $ \inp -> [(val, inp)]

zero :: Parser a
zero = Parser $ const []

item :: Parser Char
item = Parser parseItem
  where
    parseItem [] = []
    parseItem (x : xs) = [(x, xs)]

sat :: (Char -> Bool) -> Parser Char
sat p = item >>= \x -> if p x then result x else zero

digit :: Parser Char
digit = sat isDigit

lletra :: Parser Char
lletra = sat (\x -> elem x ['a'..'z'] || elem x ['A'..'Z'])

mul :: Parser a -> Parser [a]
mul p = do
  x  <- p -- apply p once
  xs <- mul p -- recursively apply `p` as many times as possible
  return (x : xs)
  <|> return []

mes :: Parser a -> Parser [a]
mes p = do
  x <- p
  xs <- mul p
  return (x : xs)

--Gramàtica

paraulesProhibides :: [String]
paraulesProhibides = ["or", "and", "not"]

anyChar :: Parser Char
anyChar = item

nomVariable :: Parser String
nomVariable = token $ do
    x <- lletra
    xs <- mul (lletra <|> digit)
    if elem (x:xs) paraulesProhibides then empty
    else return (x:xs)

stringMatch :: String -> Parser String
stringMatch (c:str) = do
    x <- sat (== c) 
    xs <- stringMatch str
    return (x:xs)
stringMatch [] = do return ""

espais :: Parser String
espais = mul (sat (\x -> x == ' ' || x == '\n' || x == '\t' ))

token :: Parser a -> Parser a
token p = espais *> p <* espais

intToken :: Parser Int 
intToken = token $ do
    s <- mes digit
    return (read s)

addSubParser :: Parser OpType
addSubParser = (token (sat (== '+')) >> return Add)
           <|> (token (sat (== '-')) >> return Sub)

mulDivParser :: Parser OpType
mulDivParser = (token (sat (== '*')) >> return Mul)
           <|> (token (sat (== '/')) >> return Div)

compParser :: Parser OpType
compParser =  (token (stringMatch "<=") >> return Leq) 
          <|> (token (stringMatch ">=") >> return Geq)
          <|> (token (stringMatch "==") >> return Eq )
          <|> (token (sat (== '<'))     >> return Lt )
          <|> (token (sat (== '>'))     >> return Gt )

expr :: Parser Expr
expr = lam  <|> logicOr -- <|> ifThenElse

-- Inici jerarquia 
logicOr :: Parser Expr
logicOr = do
    t1 <- logicAnd
    (do 
        token (stringMatch "or")
        e2 <- logicOr
        return (App (App (Op Or) t1) e2)
        ) <|> return t1

logicAnd :: Parser Expr
logicAnd = do
    t1 <- comp
    (do token (stringMatch "and")
        e2 <- logicAnd
        return (App (App (Op And) t1) e2)
        ) <|> return t1

comp :: Parser Expr
comp = do
    t1 <- suma
    (do o <- compParser
        e2 <- logicAnd
        return (App (App (Op o) t1) e2)
        ) <|> return t1

suma :: Parser Expr
suma = do
    t1 <- term
    (do o <- addSubParser
        e2 <- suma
        return (App (App (Op o) t1) e2)
        ) <|> return t1

term :: Parser Expr
term = do
    a1 <- ap
    (do o <- mulDivParser
        t2 <- term
        return (App (App (Op o) a1) t2)
        ) <|> return a1

ap :: Parser Expr
ap = do
    x <- unari
    ys <- mul unari
    return (insert (x:ys))
        where
            insert :: [Expr] -> Expr
            insert [e] = e
            insert exprs = (App (insert (init exprs)) (last exprs))

unari :: Parser Expr
unari = 
    (do (token (sat (== '-')))
        x <- unari
        return (App (App (Op Sub) (Val 0)) x) )
    <|> (do
        (token (stringMatch "not"))
        x <- unari
        return (App (Op Not) x))
    <|> atom

atom :: Parser Expr
atom = (token (sat (== '(')) *> expr <* token (sat (== ')')))
    <|> do
    x <- intToken
    return (Val x)
    <|> do
    y <- nomVariable
    return (Var y)

lam :: Parser Expr
lam = do 
    _ <- token (sat (== '\\'))
    vars <- mes nomVariable
    _ <- token (stringMatch "->")
    exp <- token expr
    return (insert vars exp)
        where
            insert :: [String] -> Expr -> Expr
            insert [a] e = (Lam a e)
            insert (v:vrs) e = (Lam v (insert vrs e))
