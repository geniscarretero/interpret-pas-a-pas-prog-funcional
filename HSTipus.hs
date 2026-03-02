module HSTipus where

data Expr = Val Int                 -- Un número (42)
          | Var String              -- Una variable (x, y, suma)
          | Op BinOp Expr Expr      -- Una operació (1 + 2)
          | Not Expr                -- Not
          | App Expr Expr           -- Aplicació de funció (f x)
          | Lam String Expr       -- Funció lambda (\x -> x + 1)
          deriving (Show)

data BinOp = Add | Sub | Mul | Div          -- Aritmètiques
            | And | Or                      -- Lògiques
            | Lt | Leq | Gt | Geq | Eq      -- Comparacions
            deriving (Show)
