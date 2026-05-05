module HSTipus where
import qualified Data.IntMap as IM

data Expr = Val Int                 -- Un número (42)
          | Var String              -- Una variable (x, y, suma, not, +, *)
          | App Expr Expr           -- Aplicació de funció (f x)
          | Lam String Expr       -- Funció lambda (\x -> x + 1)
          deriving (Show)

-- Addr són els IDs dels nodes
type Addr = Int

-- Heap és un graf dirigit, pot no ser connex
-- Com és un IntMap, necessita un addr per saber l'arrel (on comença el graf)
type Heap = IM.IntMap Node

-- HeapState té la següent adreça disponible per afegir nodes i el graf total
type HeapState = (Addr, Heap)

data Node = NVal Int                -- Int = valor
          | NVar String
          | NApp Addr Addr          -- Int = Addr
          | NLam String Addr        -- Int = Addr
          deriving (Show)

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
