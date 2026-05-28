module HSEval where

import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap as IM
import HSTipus
import HSDef
import GHC.Exception (fromCallSiteList)
import Foreign (fillBytes)

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
    (a, (a22, IM.insert a (NApp a1 a2) hs2))

ast2graph (Lam str e) (a, graph) strAddr =
  let 
    (a1, (a11,hs1)) = ast2graph e (a+2, graph) ((str,(a+1)):strAddr)
  in
    (a, (a11, IM.insert a (NLam (a+1) a1) (IM.insert (a+1) (NVar str) hs1)))

ast2graph  (If e1 e2 e3) (a, graph) strAddr =
  let 
    (a1, (aa1, hs1)) = ast2graph e1 (a+1, graph) strAddr
    (a2, (aa2, hs2)) = ast2graph e2 (aa1, hs1) strAddr
    (a3, (aa3, hs3)) = ast2graph e3 (aa2, hs2) strAddr
  in (a, (aa3, (IM.insert a (NIf a1 a2 a3) hs3)))

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
    NVar str -> case HM.lookup str preludeTypeEnv of
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
          True  -> if nApps >= nPars then False else True
          False -> False
      )
    NIf _ _ _ -> False 

-- Precondicions:
-- les adreces ja ho tenen tot avaluat en 

compute :: Primitive -> [Addr] -> HeapState -> Expr
compute Plus [a1, a2] (aa,hp) =
  let
    Just (NVal n1) = IM.lookup a1 hp
    Just (NVal n2) = IM.lookup a2 hp
  in 
    Val (n1+n2)

compute Minus [a1, a2] (aa,hp) =
  let
    Just (NVal n1) = IM.lookup a1 hp
    Just (NVal n2) = IM.lookup a2 hp
  in 
    Val (n1-n2)

compute Mult [a1, a2] (aa,hp) =
  let
    Just (NVal n1) = IM.lookup a1 hp
    Just (NVal n2) = IM.lookup a2 hp
  in 
    Val (n1*n2)

compute Div [a1, a2] (aa,hp) =
  let
    Just (NVal n1) = IM.lookup a1 hp
    Just (NVal n2) = IM.lookup a2 hp
  in 
    Val (div n1 n2)

compute Eq [a1, a2] (aa,hp) =
  let
    Just expr1 = IM.lookup a1 hp
    Just expr2 = IM.lookup a2 hp
  in 
    Var (if expr1==expr2 then "True" else "False")

compute Lt [a1, a2] (aa,hp) =
  let
    Just (NVal n1) = IM.lookup a1 hp
    Just (NVal n2) = IM.lookup a2 hp
  in 
    Var (if n1<n2 then "True" else "False")

compute Lte [a1, a2] (aa,hp) =
  let
    Just (NVal n1) = IM.lookup a1 hp
    Just (NVal n2) = IM.lookup a2 hp
  in 
    Var (if n1<=n2 then "True" else "False")

compute Gt [a1, a2] (aa,hp) =
  let
    Just (NVal n1) = IM.lookup a1 hp
    Just (NVal n2) = IM.lookup a2 hp
  in 
    Var (if n1>n2 then "True" else "False")

compute Gte [a1, a2] (aa,hp) =
  let
    Just (NVal n1) = IM.lookup a1 hp
    Just (NVal n2) = IM.lookup a2 hp
  in 
    Var (if n1>=n2 then "True" else "False")

compute And [a1, a2] (aa,hp) =
  let
    Just (NVar n1) = IM.lookup a1 hp
    Just (NVar n2) = IM.lookup a2 hp
    b1 = read n1 :: Bool
    b2 = read n2 :: Bool
  in 
    Var (if b1 && b2 then "True" else "False")

compute Or [a1, a2] (aa,hp) =
  let
    Just (NVar n1) = IM.lookup a1 hp
    Just (NVar n2) = IM.lookup a2 hp
    b1 = read n1 :: Bool
    b2 = read n2 :: Bool
  in 
    Var (if b1 || b2 then "True" else "False")


-- Un pas en el procés d'avaluar
-- Int = quantes aplicacions s'han de treure?
--  indica que, si s'ha fet una aplicació s'ha de canviar l'adreça per la nova que tha donat
--  Es pot treure el bool de retorn, ho puc fer directament abans del backtrack
pas :: (Addr, HeapState) -> DefEnv -> [Addr] -> Either String HeapState
pas (a, (aa,hp)) defEnv stk =
  let Just n = IM.lookup a hp
  in case n of
    NLam a1 a2 -> (
      let 
        (app1:_) = stk
        -- el que apunta a a1 ha de ser el contingut de a3
        Just (NApp _ a3) = IM.lookup app1 hp
        Just contingut = IM.lookup a3 hp
        -- El que m'apuntava a mi, ha d'apuntar a a2
        Just brancaDretaLam = IM.lookup a2 hp 

        -- s'ha d'esborrar a3
        -- S'ha de substituir:
        --  app1 pel contingut d'a2
        --  a1 pel contingut d'a3
        newHp = IM.insert app1 brancaDretaLam (IM.delete app1 (IM.delete a (IM.insert a1 contingut (IM.delete a1 (IM.delete a3 hp))))) 

      in Right (aa,newHp)
      ) 
    NVar s -> (
      case HM.lookup s defEnv of 
        Just code -> (case code of 
          PrimDef opt arity -> (
            let
              (app1:app2:_) = stk
              Just (NApp _ p1) = IM.lookup app1 hp
              Just (NApp _ p2) = IM.lookup app2 hp
            in
              -- Primer cas, el primer paràmetre no esta en WHNF, és a dir, es pot fer algun pas 
              if not (isWHNF (p1, (aa,hp))) 
                then pas (p1, (aa,hp)) defEnv [] -- pas fet
                else (
                  -- Segon cas, el segon (i últim) paràmetre no esta en WHNF, és a dir, es pot fer algun pas
                  if not (isWHNF (p2, (aa,hp))) 
                    then pas (p2, (aa,hp)) defEnv [] -- pas fet
                    else 
                    (
                      -- Els dos estan en WHNF suposadament seràn o numeros o True/False
                      let 
                        (addrNew, (aa1,hp1)) = ast2graph (compute opt [p1,p2] (aa, hp)) (aa,hp) []
                        -- Un cop creat el graph:
                        -- el que apuntava a app1 ha d'apuntar a aquest nou graph
                        Just contingut = IM.lookup addrNew hp1
                        -- Per tant: eliminar app1, app2 i addrnew
                        -- No puc eliminar fills app1 i app2 pq no sé si es tornen a utilitzar (implementar gc en IM)
                        -- substituir app2 amb contingut nou
                        newHp = IM.insert app2 contingut (IM.delete addrNew (IM.delete a (IM.delete app2 (IM.delete app1 hp1))))
                      in 
                        Right (aa1,newHp) -- Pas fet
                    )
                  )
            )
            
          FuncDef expr -> (
            let 
              (addr, (aa1, hp1)) = ast2graph expr (aa,hp) [] -- Substitueixo pel graph ja fet del supercombinador (pas fet)
              Just contingut = IM.lookup addr hp1
              --Eliminar nou node i substituir per el que ens apunta a nosaltress
            in Right(aa1, (IM.insert a contingut (IM.delete a (IM.delete addr hp1))))
            )
          )
        Nothing -> Left "Variable no trobada" 
        )
    NVal _ -> Right (aa,hp) -- Hauria de petar
    NApp a1 a2 -> pas (a1, (aa,hp)) defEnv (a:stk)
    NIf a1 a2 a3 -> if not (isWHNF (a1,(aa,hp)) ) 
      then pas (a1, (aa, hp)) defEnv []
      else case IM.lookup a1 hp of 
        Just (NVar "True") ->( 
          let Just contingut = IM.lookup a2 hp
          in Right (aa, IM.insert a contingut (IM.delete a hp))
          )
        Just (NVar "False") ->( 
          let Just contingut = IM.lookup a3 hp
          in Right (aa, IM.insert a contingut (IM.delete a hp))
          )

debugEvalLoop :: (Addr,HeapState) -> Int -> Either String (Addr, HeapState)
debugEvalLoop graph 0 = Right graph 
debugEvalLoop graph@(addr, _) (-1) =
  if isWHNF graph then Right graph
    else
      do 
        next <- pas graph preludeDefEnv []
        hs <- debugEvalLoop (addr, next) (-1) 
        return hs

debugEvalLoop graph@(addr, _) n = 
  if isWHNF graph then Right graph
    else
      do
        next <- pas graph preludeDefEnv []
        hs <- debugEvalLoop (addr, next) (n-1) 
        return hs

showGraph :: (Addr, HeapState) -> String
showGraph (a, hs@(aa,hp)) =
  let Just n = IM.lookup a hp
  in 
    case n of 
      NVal v -> "Val " ++ show v
      NVar str -> "Var " ++ "\"" ++ str ++ "\""
      NApp a1 a2 -> "App (" ++ showGraph (a1, hs) ++ ") (" ++ showGraph (a2, hs) ++ ")"
      NLam var a1 -> "Lam (" ++ showGraph (var, hs)  ++ ") (" ++ showGraph (a1, hs) ++ ")"
    

