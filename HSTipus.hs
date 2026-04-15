module HSTipus where

data Expr = Val Int                 -- Un número (42)
          | Var String              -- Una variable (x, y, suma)
          | Op OpType                 -- Una operació (1 + 2) o not (1 < x)
          | App Expr Expr           -- Aplicació de funció (f x)
          | Lam String Expr       -- Funció lambda (\x -> x + 1)
          deriving (Show)

data OpType = Add | Sub | Mul | Div          -- Aritmètiques
            | And | Or                      -- Lògiques
            | Lt | Leq | Gt | Geq | Eq      -- Comparacions
            | Not
            deriving (Show)

data Tipus = TInt
           | TBool
           | TFun Tipus Tipus
           | TVar String
           deriving (Show)

showTipus :: Tipus -> String
showTipus TInt = "Int"
showTipus TBool = "Bool"
showTipus (TFun t1 t2) = showTipus t1 ++ " -> " ++ showTipus t2
showTipus (TVar a) = "\""++a++"\""
