module HSInferenciaTipus where

import HSTipus

-- Context és [(nom_variable, tipus)]
type Context = [(String, Tipus)]

-- Subst és [(nom_variable_tipus, tipus)]
-- sempre serà un nom de variable que haguem generat
type Subst = [(String, Tipus)]

-- Donats un conjunt de substitucions i un tipus, se li aplica, en cas de que sigui necessari alguna de les substitucions en aquest tipus
aplicaSubst :: Subst -> Tipus -> Tipus
aplicaSubst s (TVar v) = case lookup v s of
                            Just t  -> aplicaSubst s t -- Continuem buscant per si t és una altra TVar
                            Nothing -> TVar v
aplicaSubst s (TFun t1 t2) = TFun (aplicaSubst s t1) (aplicaSubst s t2)
aplicaSubst _ t = t -- TInt, TBool es queden igual

-- Donats un conjunt de substitucions i un context, s'apliquen les substitucions en el context i es retorna.
aplicaSubstCtx :: Subst -> Context -> Context
aplicaSubstCtx _ [] = []
aplicaSubstCtx subs ((st,t):ctx) = ((st, aplicaSubst subs t): (aplicaSubstCtx subs ctx)) 

--Donats dos tipus s'obté el conjunt de substitucions necessàries pq siguin el mateix tipus
-- t1 == t2
generaSubst :: Tipus -> Tipus -> Either String Subst
generaSubst TInt TBool = Left "No es pot unificar un enter amb un booleà"
generaSubst TBool TInt = Left "No es pot unificar un booleà amb un enter"
generaSubst (TFun t1 t2) (TFun t3 t4) = do 
  first <- generaSubst t1 t3
  second <- generaSubst t2 t4
  return (first ++ second)
generaSubst (TVar s1) (TVar s2) = Right [(s1, (TVar s2)), (s2, (TVar s1))]
generaSubst (TVar s) t = Right [(s, t)]
generaSubst t (TVar s) = Right [(s, t)]
generaSubst _ _ = Right []

-- Donats dos tipus se suposa que s'ha de fer una aplicació (l'esquerra s'aplica al dret)
-- Sempre que l'esquerra no sigui una funció, serà una aplicació incorrecta
checkAppTypes :: Tipus -> Tipus -> Either String String
checkAppTypes (TFun _ _) _ = Right "OK"
checkAppTypes _ _ = Left "Estàs intentant fer una aplicació a una cosa que no és una funció" 

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
                ("not", TFun TBool TBool),
                ("id", TFun (TVar "a") (TVar "a")),
                ("const", TFun (TVar "a") (TFun (TVar "b") (TVar "a")))
                ]


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
                  Nothing -> Left "Variable no trobada"
                  Just t -> Right (t, [], n)

infereix ctx (Lam s e) n = do
  t_s <- Right (TVar ("t"++ show n ))                     -- Tipus de variable de lambda (b)
  (t1, s1, n1) <- infereix ((s,t_s):ctx) e (n+1)          -- Tipus del cos de lambda (c)
  t_retorn <- Right (aplicaSubst s1 (TFun t_s t1))        -- a = b -> c
  return (t_retorn, s1, n1)
        
        
infereix ctx (App e1 e2) n = do
  (t1, s1, n1) <- infereix ctx e1 n                       -- Tipus del cos de l'aplicació (b)
  (t2, s2, n2) <- infereix (aplicaSubstCtx s1 ctx) e2 n1  -- Tipus del cos de lambda (c)
  
  checkAppTypes (aplicaSubst s2 t1) t2                    -- Comprova que es pugui fer l'aplicació

  t3 <- Right (TVar ("t" ++ show n2))                     -- Tipus de l'aplicació (a)

  s3 <- generaSubst (aplicaSubst s2 t1) (TFun t2 t3)      -- b = c -> a

  return ((aplicaSubst s3 t3), (s1++s2++s3), (n2+1))      -- Retorna el tipus de l'aplicació ja substituit
