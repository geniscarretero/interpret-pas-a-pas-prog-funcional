module HSDef where
import HSTipus
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap as IM

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
          | NLam Addr Addr        -- Int = Addr
          | NIf Addr Addr Addr
          deriving (Show,Eq)

-- 1. Un llistat de les teves primitives nuclears
data Primitive = Plus | Minus | Mult | Div | Eq | Lt | Lte | Gt | Gte | And | Or
  deriving (Show, Eq)

-- 2. Què pot haver-hi guardat en el llistat global?
data EnvObj 
  = PrimDef Primitive Int    -- La primitiva i la seva aritat (quants arguments demana)
  | FuncDef Expr    -- Una funció pròpia: codi AST (cos)
  deriving (Show)

type DefEnv = HM.HashMap String EnvObj

preludeDefEnv :: DefEnv
preludeDefEnv = HM.fromList
  -- Primitives (Nom, Constructor, Aritat)
  [ ("+",     PrimDef Plus 2)
  , ("-",     PrimDef Minus 2)
  , ("*",     PrimDef Mult 2)
  , ("/",     PrimDef Div 2)
  , ("==",     PrimDef Eq 2)
  , ("<",     PrimDef Lt 2)
  , ("<=",     PrimDef Lte 2)
  , (">",     PrimDef Gt 2)
  , (">=",     PrimDef Gte 2)
  , ("&&",     PrimDef And 2)
  , ("||",     PrimDef Or 2)
  , ("True", FuncDef (Var "True"))
  , ("False", FuncDef (Var "False"))
 -- Les funcions predefinides ja les carregaré amb un codi previ 
  ]


