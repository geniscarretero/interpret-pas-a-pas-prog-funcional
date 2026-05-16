module HSEval where

import qualified Data.IntMap as IM
import HSTipus
import HSFuncs

--Retorna un parell: l'Addr de l'arrel de la expr que li passes i el heapstate (Següent addr disponible, graf total)
ast2graph :: Expr -> HeapState -> [(String,Addr)] -> (Addr, HeapState)   -- Expr -> estat -> [(noms de variables a compartir, seves posicions al heap)]-> (Addr, HeapState) 
ast2graph (Val v) (a, graph) _ = (a, (a+1, IM.insert a (NVal v) graph))
ast2graph (Var s) (a, graph) strAddr = 
  case lookup s strAddr of
    Just addr -> (addr, (a, graph))
    Nothing -> (a, (a+1, IM.insert a (NVar s) graph))

ast2graph (App e1 e2) (a, graph) strAddr =
  let 
    (a1, (a11,hs1)) = ast2graph e1 (a+1, graph) strAddr
    (a2, (a22,hs2)) = ast2graph e2 (a11,hs1) strAddr
  in
    (a, (a22, IM.insert a (NApp a1 a2) (IM.union hs1 hs2)))

ast2graph (Lam str e) (a, graph) strAddr =
  let 
    (a1, (a11,hs1)) = ast2graph e (a+2, graph) ((str,(a+1)):strAddr)
  in
    (a, (a11, IM.insert a (NLam str a1) (IM.insert (a+1) (NVar str) hs1)))

-- Comencem AVALUACIÓ
-- Bucle
  -- isWHNF?
    -- Sí: acabar
    -- No:
      -- B-reduccions
      -- app
      -- funcions primitives

getNumParams :: Tipus -> Int -> Int
getNumParams t n =
  case t of 
    TFun _ t2 -> getNumParams t2 (n+1)
    _ -> n

getNumAppOfPredFunc :: (Addr,HeapState) -> Int -> (Bool, Int, Int)
getNumAppOfPredFunc (a, (aa,hp)) i = 
  let Just n = IM.lookup a hp
  in case n of 
    NVal _ -> (False, i, 0)
    NVar str -> case lookup str funcionsPredefinides of
      Nothing -> (False, i, 0)
      Just t -> (True, i, (getNumParams t 0))
    NLam _ _ -> (False, i, 0)
    NApp a1 _ -> getNumAppOfPredFunc (a1,(aa,hp)) (i+1)

-- WHNF:
--Weak Head Normal Form (WHNF): una expressió està en WHNF si és:
--    literal, variable, una abstracció lambda (λx.(+) x 1), ó
--    funció predefinida parcialment aplicada ((+) ((-) 4 3)).

-- Bàsicament rep el graf i diu si és WHNF o no
isWHNF :: (Addr, HeapState) -> Bool 
isWHNF (a, (aa,hp)) =
  let Just n = IM.lookup a hp
  in case n of
    NVal _ -> True
    NVar _ -> True
    NLam _ _ -> True
    NApp _ _ -> (
      let 
        (b1, nApps, nPars) = getNumAppOfPredFunc (a, (aa,hp)) 0
      in case b1 of
          True  -> if nApps == nPars then  False else True
          False -> False
      )

-- No caldrà en principi
showGraph :: (Addr, HeapState) -> String
showGraph (a, (aa,hs)) =
  let Just n = IM.lookup a hs
  in 
    case n of 
      NVal v -> "Val " ++ show v
      NVar str -> "Var " ++ "\"" ++ str ++ "\""
      NApp a1 a2 -> "App (" ++ showGraph (a1, (aa,hs)) ++ ") (" ++ showGraph (a2, (aa,hs)) ++ ")"
      NLam str a1 -> "Lam \"" ++ str ++ "\" (" ++ showGraph (a1, (aa,hs)) ++ ")"
    

