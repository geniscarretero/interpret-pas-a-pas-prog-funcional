module HSInferenciaTipus where

import HSTipus

count :: Int          --Comptador per anomenar variables
count = 0

type Context = [(String, Tipus)]
type Subst = [(String, Tipus)]

aplicaSubst :: Subst -> Tipus -> Tipus
aplicaSubst s (TVar v) = case lookup v s of
                            Just t  -> aplicaSubst s t -- Continuem buscant per si t és una altra TVar
                            Nothing -> TVar v
aplicaSubst s (TFun t1 t2) = TFun (aplicaSubst s t1) (aplicaSubst s t2)
aplicaSubst _ t = t -- TInt, TBool es queden igual
  

aplicaSubstCtx :: Subst -> Context -> Context
aplicaSubstCtx _ [] = []
aplicaSubstCtx subs ((st,t):ctx) = ((st, aplicaSubst subs t): (aplicaSubstCtx subs ctx)) 

unifica :: Tipus -> Tipus -> Either String Subst
unifica TInt TBool = Left "No es pot unificar un enter amb un booleà"
unifica TBool TInt = Left "No es pot unificar un booleà amb un enter"
unifica (TFun t1 t2) (TFun t3 t4) = do 
  first <- unifica t1 t3
  second <- unifica t2 t4
  return (first ++ second)

unifica (TVar s) t = Right [(s, t)]
unifica t (TVar s) = Right [(s, t)]
unifica _ _ = Right []

envInicial :: Context
envInicial = [  ("+", TFun TInt (TFun TInt TInt)),
                ("-", TFun TInt (TFun TInt TInt)),
                ("/", TFun TInt (TFun TInt TInt)),
                ("*", TFun TInt (TFun TInt TInt)),
                ("<", TFun TInt (TFun TInt TBool)),
                (">", TFun TInt (TFun TInt TBool)),
                ("<=", TFun TInt (TFun TInt TBool)),
                (">=", TFun TInt (TFun TInt TBool)),
                ("==", TFun TInt (TFun TInt TBool)),
                ("and", TFun TBool (TFun TBool TBool)),
                ("or", TFun TBool (TFun TBool TBool)),
                ("not", TFun TBool TBool)]


infereix :: Context -> Expr -> Int -> Either String (Tipus, Subst, Int)
infereix _ (Val _) i = Right (TInt, [], i) 
infereix _ (Op Add) i = Right ((let Just x = (lookup "+" envInicial) in x), [], i)
infereix _ (Op Sub) i = Right ((let Just x = (lookup "-" envInicial) in x), [], i)
infereix _ (Op Div) i = Right ((let Just x = (lookup "/" envInicial) in x), [], i)
infereix _ (Op Mul) i = Right ((let Just x = (lookup "*" envInicial) in x), [], i)
infereix _ (Op Lt) i = Right ((let Just x = (lookup "<" envInicial) in x), [], i)
infereix _ (Op Gt) i = Right ((let Just x = (lookup ">" envInicial) in x), [], i)
infereix _ (Op Leq) i = Right ((let Just x = (lookup "<=" envInicial) in x), [], i)
infereix _ (Op Geq) i = Right ((let Just x = (lookup ">=" envInicial) in x), [], i)
infereix _ (Op Eq) i = Right ((let Just x = (lookup "==" envInicial) in x), [], i)
infereix _ (Op And) i = Right ((let Just x = (lookup "and" envInicial ) in x), [], i)
infereix _ (Op Or) i = Right ((let Just x = (lookup "or" envInicial ) in x), [], i)
infereix _ (Op Not) i = Right ((let Just x = (lookup "not" envInicial ) in x), [], i)

infereix ctx (Var a) n = case lookup a ctx of
                  Nothing -> Right ((TVar ("t"++show n)), [], n+1)  --no hauria de passar
                  Just t -> Right (t, [], n)

infereix ctx (Lam s e) n =
  let
    t1 = infereix ((s, (TVar ("t"++show n))):ctx) e n
  in
    case t1 of
      Left text -> Left text -- Propaguem error amunt
      Right (t, s, i) -> Right ((TFun (TVar ("t" ++ show n)) t ), s, i+1)

infereix ctx (App e0 e1) n = do
  (t0, s0, n1) <- infereix ctx e0 n                      -- 1. Analitzem funció
  (t1, s1, n2) <- infereix (aplicaSubstCtx s0 ctx) e1 n1  -- 2. Analitzem argument amb el que hem après a s0
  
  t_retorn <- Right (TVar ("t" ++ show n2))                     -- 3. Nou nom per al resultat
  
  -- 4. Unifiquem: el tipus de la funció (t0) ha de ser igual a (t1 -> t_retorn)
  s2 <- unifica (aplicaSubst s1 t0) (TFun t1 t_retorn)
  
  -- 5. Juntem totes les substitucions
  let s_final = s2 ++ s1 ++ s0
  return (aplicaSubst s2 t_retorn, s_final, n2 + 1)  
  

