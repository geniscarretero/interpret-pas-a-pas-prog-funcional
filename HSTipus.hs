module HSTipus where
import qualified Data.IntMap as IM
import qualified Data.HashMap.Strict as HM

data Expr = Val Int                 -- Un número (42)
          | Var String              -- Una variable (x, y, suma, not, +, *)
          | App Expr Expr           -- Aplicació de funció (f x)
          | Lam String Expr       -- Funció lambda (\x -> x + 1)
          | If Expr Expr Expr
          deriving (Show)

data Binding = Bind String Expr

data Line = Expr | Binding

type TypeEnv = HM.HashMap String Tipus

funcionsPredefinides = [  ("+", TFun TInt (TFun TInt TInt)),
                ("-", TFun TInt (TFun TInt TInt)),
                ("/", TFun TInt (TFun TInt TInt)),
                ("*", TFun TInt (TFun TInt TInt)),
                ("<", TFun TInt (TFun TInt TBool)),
                (">", TFun TInt (TFun TInt TBool)),
                ("<=", TFun TInt (TFun TInt TBool)),
                (">=", TFun TInt (TFun TInt TBool)),
                ("==", TFun TInt (TFun TInt TBool)),
                ("&&", TFun TBool (TFun TBool TBool)),
                ("||", TFun TBool (TFun TBool TBool)),
                --Esborrar perquè les afegire al preludi del llenguatge
                ("id", TFun (TVar "a") (TVar "a")),
                ("const", TFun (TVar "a") (TFun (TVar "b") (TVar "a"))),
                ("not", TFun (TBool) (TBool)),
                (".", TFun (TFun (TVar "b") (TVar "c"))(TFun (TFun (TVar "a") (TVar "b")) (TFun (TVar "a") (TVar "c")))),
                ("True", TBool),
                ("False", TBool)
                ]
 

preludeTypeEnv :: TypeEnv 
preludeTypeEnv = HM.fromList funcionsPredefinides

data Tipus = TInt
           | TBool
           | TFun Tipus Tipus
           | TVar String
           deriving (Show)

showTipus :: Tipus -> String
showTipus TInt = "Int"
showTipus TBool = "Bool"
showTipus (TFun (TFun t1 t2) t3) = "(" ++ showTipus t1 ++ " -> " ++ showTipus t2 ++ ")" ++ " -> " ++ showTipus t3 
showTipus (TFun t1 t2) = showTipus t1 ++ " -> " ++ showTipus t2
showTipus (TVar a) = a
