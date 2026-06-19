module HSEval where

import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap as IM
import HSTipus
import HSDef
import GHC.Exception (fromCallSiteList)
import Foreign (fillBytes)
import GHC.Integer (complementInteger)
import Data.Char (isLetter)

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


graph2ast :: (Addr, HeapState) -> Expr
graph2ast (a, hs@(_,hp)) =
  case IM.lookup a hp of
    Just (NVar s) -> (Var s)
    Just (NVal i) -> (Val i)
    Just (NApp a1 a2) -> (App (graph2ast (a1,hs)) (graph2ast (a2,hs)))
    Just (NLam a1 a2) -> (Lam str (graph2ast (a2,hs)))
      where 
        Just (NVar str) = IM.lookup a1 hp
    Just (NIf a1 a2 a3) -> (If (graph2ast (a1,hs)) (graph2ast (a2,hs)) (graph2ast (a3,hs)) )

copyGraph :: (Addr,HeapState) -> [(String,Addr)] -> (Addr, HeapState)   -- Expr -> estat -> [(noms de variables a compartir, seves posicions al heap)]-> (Addr, HeapState) 
copyGraph (addr, (a, heap)) strAddr =
  case IM.lookup addr heap of

    Just (NVal v) -> (a, (a+1, IM.insert a (NVal v) heap))

    Just (NVar s) -> 
      case lookup s strAddr of
        Just addr -> (addr, (a, heap))
        Nothing -> (a, (a+1, IM.insert a (NVar s) heap))

    Just (NApp e1 e2)->
      let 
        (a1, (a11,hs1)) = copyGraph (e1,(a+1, heap)) strAddr
        (a2, (a22,hs2)) = copyGraph (e2,(a11,hs1)) strAddr
      in
        (a, (a22, IM.insert a (NApp a1 a2) hs2))

    Just (NLam aStr e) ->
      let 
        Just (NVar str) = IM.lookup aStr heap
        (a1, (a11,hs1)) = copyGraph (e, (a+2, heap)) ((str,(a+1)):strAddr)
      in
        (a, (a11, IM.insert a (NLam (a+1) a1) (IM.insert (a+1) (NVar str) hs1)))

    Just (NIf e1 e2 e3) ->
      let 
        (a1, (aa1, hs1)) = copyGraph (e1, (a+1, heap)) strAddr
        (a2, (aa2, hs2)) = copyGraph (e2, (aa1, hs1)) strAddr
        (a3, (aa3, hs3)) = copyGraph (e3, (aa2, hs2)) strAddr
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

getNumAppOfPredFunc :: (Addr,HeapState) -> Int -> TypeEnv -> (Bool, Int, Int)
getNumAppOfPredFunc (a, (aa,hp)) i  typeEnv= 
  let Just n = IM.lookup a hp
  in case n of 
    NVal _ -> (False, i, 0)
    NVar str -> case HM.lookup str typeEnv of
      Nothing -> (False, i, 0)
      Just t -> (True, i, (getNumParams t 0))
    NLam _ _ -> (False, i, 0)
    NApp a1 _ -> getNumAppOfPredFunc (a1,(aa,hp)) (i+1) typeEnv

-- WHNF:
--Weak Head Normal Form (WHNF): una expressió està en WHNF si és:
--    literal, variable, una abstracció lambda (λx.(+) x 1), ó
--    funció predefinida parcialment aplicada ((+) ((-) 4 3)).

-- Bàsicament rep el graf i diu si és WHNF o no
isWHNF :: (Addr, HeapState) -> TypeEnv -> Bool 
isWHNF (a, (aa,hp)) typeEnv =
  let Just n = IM.lookup a hp
  in case n of
    NVal _ -> True
    NVar _ -> True
    NLam _ _ -> True
    NApp _ _ -> (
      let 
        (b1, nApps, nPars) = getNumAppOfPredFunc (a, (aa,hp)) 0 typeEnv
      in case b1 of
          True  -> if nApps >= nPars then False else True
          False -> False
      )
    NIf _ _ _ -> False 

-- Precondicions:
-- les adreces ja ho tenen tot avaluat en 

compute :: Primitive -> [Addr] -> HeapState -> Either String Expr
compute Plus [a1, a2] (aa,hp) =
  let
    Just (NVal n1) = IM.lookup a1 hp
    Just (NVal n2) = IM.lookup a2 hp
  in 
    Right(Val (n1+n2))

compute Minus [a1, a2] (aa,hp) =
  let
    Just (NVal n1) = IM.lookup a1 hp
    Just (NVal n2) = IM.lookup a2 hp
  in 
    Right(Val (n1-n2))

compute Mult [a1, a2] (aa,hp) =
  let
    Just (NVal n1) = IM.lookup a1 hp
    Just (NVal n2) = IM.lookup a2 hp
  in 
    Right(Val (n1*n2))

compute Div [a1, a2] (aa,hp) =
  let
    Just (NVal n1) = IM.lookup a1 hp
    Just (NVal n2) = IM.lookup a2 hp
  in 
    if n2 == 0 then Left ("Divisió entre 0")
      else 
    Right(Val (div n1 n2))

compute Eq [a1, a2] (aa,hp) =
  let
    Just expr1 = IM.lookup a1 hp
    Just expr2 = IM.lookup a2 hp
  in 
    Right(Var (if expr1==expr2 then "True" else "False"))

compute Lt [a1, a2] (aa,hp) =
  let
    Just (NVal n1) = IM.lookup a1 hp
    Just (NVal n2) = IM.lookup a2 hp
  in 
    Right(Var (if n1<n2 then "True" else "False"))

compute Lte [a1, a2] (aa,hp) =
  let
    Just (NVal n1) = IM.lookup a1 hp
    Just (NVal n2) = IM.lookup a2 hp
  in 
    Right(Var (if n1<=n2 then "True" else "False"))

compute Gt [a1, a2] (aa,hp) =
  let
    Just (NVal n1) = IM.lookup a1 hp
    Just (NVal n2) = IM.lookup a2 hp
  in 
    Right(Var (if n1>n2 then "True" else "False"))

compute Gte [a1, a2] (aa,hp) =
  let
    Just (NVal n1) = IM.lookup a1 hp
    Just (NVal n2) = IM.lookup a2 hp
  in 
    Right(Var (if n1>=n2 then "True" else "False"))

compute And [a1, a2] (aa,hp) =
  let
    Just (NVar n1) = IM.lookup a1 hp
    Just (NVar n2) = IM.lookup a2 hp
    b1 = read n1 :: Bool
    b2 = read n2 :: Bool
  in 
    Right(Var (if b1 && b2 then "True" else "False"))

compute Or [a1, a2] (aa,hp) =
  let
    Just (NVar n1) = IM.lookup a1 hp
    Just (NVar n2) = IM.lookup a2 hp
    b1 = read n1 :: Bool
    b2 = read n2 :: Bool
  in 
    Right(Var (if b1 || b2 then "True" else "False"))


-- Un pas en el procés d'avaluar
-- Int = quantes aplicacions s'han de treure?
--  indica que, si s'ha fet una aplicació s'ha de canviar l'adreça per la nova que tha donat
--  Es pot treure el bool de retorn, ho puc fer directament abans del backtrack
pas :: (Addr, HeapState) -> TypeEnv -> DefEnv -> [Addr] -> Either String HeapState
pas (a, (aa,hp)) typeEnv defEnv stk =
  let Just n = IM.lookup a hp
  in case n of
    NLam _ _ -> (
      let 
        (app1:_) = stk
        -- fer una còpia de la lambda (perque potser algú més la torna a fer servir)
        (aNewLam, (aa1, hpNew)) = copyGraph (a, (aa,hp)) []
        Just (NLam a1 a2) = IM.lookup aNewLam hpNew
        

        -- el que apunta a a1 ha de ser el contingut de a3
        Just (NApp _ a3) = IM.lookup app1 hpNew
        Just contingut = IM.lookup a3 hpNew
        -- El que m'apuntava a mi, ha d'apuntar a a2
        hpAmbArg = IM.insert a1 contingut hpNew 
        Just brancaDretaLam = IM.lookup a2 hpAmbArg 

        -- s'ha d'esborrar a3
        -- S'ha de substituir:
        --  app1 pel contingut d'a2
        --  a1 pel contingut d'a3
        newHp = IM.insert app1 brancaDretaLam hpAmbArg 

      in Right (aa1,newHp)
      ) 
    NVar s -> 
      if s == "False" || s == "True" then Left "Aplicació no vàlida"
        else
      (
      case HM.lookup s defEnv of 
        Just code -> (case code of 
          PrimDef opt arity -> (
            let
              (app1:app2:_) = stk
              Just (NApp _ p1) = IM.lookup app1 hp
              Just (NApp _ p2) = IM.lookup app2 hp
            in
              -- Primer cas, el primer paràmetre no esta en WHNF, és a dir, es pot fer algun pas 
              if not (isWHNF (p1, (aa,hp)) typeEnv) 
                then pas (p1, (aa,hp)) typeEnv defEnv [] -- pas fet
                else (
                  -- Segon cas, el segon (i últim) paràmetre no esta en WHNF, és a dir, es pot fer algun pas
                  if not (isWHNF (p2, (aa,hp)) typeEnv) 
                    then pas (p2, (aa,hp)) typeEnv defEnv [] -- pas fet
                    else 
                    (
                      -- Els dos estan en WHNF suposadament seràn o numeros o True/False
                      case compute opt [p1, p2] (aa,hp) of 
                        Right result ->
                          let
                            (addrNew, (aa1,hp1)) = ast2graph result (aa,hp) []
                            -- Un cop creat el graph:
                            -- el que apuntava a app1 ha d'apuntar a aquest nou graph
                            Just contingut = IM.lookup addrNew hp1
                            -- Per tant: eliminar app1, app2 i addrnew
                            -- No puc eliminar fills app1 i app2 pq no sé si es tornen a utilitzar (implementar gc en IM)
                            -- substituir app2 amb contingut nou
                            newHp = IM.insert app2 contingut hp1
                          in 
                            Right (aa1,newHp) -- Pas fet
                        Left text -> Left text
                    )
                  )
            )
            
          FuncDef expr -> (
            let 
              (addr, (aa1, hp1)) = ast2graph expr (aa,hp) [] -- Substitueixo pel graph ja fet del supercombinador (pas fet)
              Just contingut = IM.lookup addr hp1
              --Eliminar nou node i substituir per el que ens apunta a nosaltress
            in Right(aa1, (IM.insert a contingut hp1))
            )
          )
        Nothing -> Left "Variable no trobada" 
        )
    NVal _ -> Left ("Aplicació no vàlida") -- Hauria de petar
    NApp a1 a2 -> pas (a1, (aa,hp)) typeEnv defEnv (a:stk)
    NIf a1 a2 a3 -> if not (isWHNF (a1,(aa,hp)) typeEnv) 
      then pas (a1, (aa, hp)) typeEnv defEnv []
      else case IM.lookup a1 hp of 
        Just (NVar "True") ->( 
          let Just contingut = IM.lookup a2 hp
          in Right (aa, IM.insert a contingut hp)
          )
        Just (NVar "False") ->( 
          let Just contingut = IM.lookup a3 hp
          in Right (aa, IM.insert a contingut hp)
          )

evalVar :: (Addr,HeapState) -> TypeEnv -> DefEnv -> [String] -> Either String ((Addr, HeapState),[String])
evalVar graph@(addr, _) typeEnv defEnv trace =
  do 
    next <- pas graph typeEnv defEnv []
    (hs,trace1) <- evalLoop (addr, next) (-1) typeEnv defEnv trace 
    return (hs, ((hsprint graph False):trace1))

evalLoop :: (Addr,HeapState) -> Int -> TypeEnv -> DefEnv -> [String] -> Either String ((Addr, HeapState),[String])
evalLoop graph 0 _ _ trace = Right (graph, ((hsprint graph False):trace)) 
evalLoop graph@(addr, _) (-1) typeEnv defEnv trace =
  if isWHNF graph typeEnv then Right (graph,((hsprint graph False):trace))
    else
      do 
        next <- pas graph typeEnv defEnv []
        (hs,trace1) <- evalLoop (addr, next) (-1) typeEnv defEnv trace 
        return (hs, ((hsprint graph False):trace1))

evalLoop graph@(addr, _) n typeEnv defEnv trace = 
  if isWHNF graph typeEnv then Right (graph,((hsprint graph False):trace))
    else
      do
        next <- pas graph typeEnv defEnv []
        (hs,trace1) <- evalLoop (addr, next) (n-1) typeEnv defEnv trace 
        return (hs, ((hsprint graph False):trace1))


hsprint :: (Addr, HeapState) -> Bool -> String
hsprint (a, hs@(aa,hp)) inLam =
  let Just n = IM.lookup a hp
  in 
    case n of 
      NVal v -> (if inLam then "-> " else "") ++ show v
      NVar str -> (if inLam then "-> " else "") ++ str
      NApp a1 a2 -> 
        let (c1,c2) = calPar a1 a2 hs
        in
          (if inLam then "-> " else "") ++
          (if c1 then "(" ++ hsprint  (a1, hs) False ++ ")" 
          else hsprint  (a1, hs) False)
          ++ " " ++
          (if c2 then "(" ++ hsprint  (a2, hs) False ++ ")" 
          else hsprint  (a2, hs) False)

      NLam var a1 -> if inLam 
        then hsprint (var, hs) False ++" " ++ hsprint (a1, hs) True
        else "(\\" ++ hsprint (var,hs) False ++ " " ++ hsprint(a1,hs) True ++ ")"
      NIf a1 a2 a3 -> (if inLam then "-> " else "") ++ "if " ++ hsprint (a1,hs) False ++ " then " ++ hsprint (a2,hs) False ++ " else " ++ hsprint (a3,hs) False

-- Si fill dret és app o lam parèntesi
-- Si fill esq és a dir una var, si la var és paraula no cal sinó si
--
calPar :: Addr -> Addr -> HeapState -> (Bool,Bool) -- Esquerra i dreta
calPar a1 a2 (_,hp) = (b1, b2)
  where
    b1 = case IM.lookup a1 hp of
      Just (NVar var) -> case var of 
        (l:_) -> not (isLetter l)
      Just _ -> False
    b2 = case IM.lookup a2 hp of
      Just (NApp _ _) -> True
      Just _ -> False

