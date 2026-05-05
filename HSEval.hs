module HSEval where

import qualified Data.IntMap as IM
import HSTipus

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
    

