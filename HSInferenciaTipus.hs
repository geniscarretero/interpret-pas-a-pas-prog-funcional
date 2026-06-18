module HSInferenciaTipus where

import HSTipus
import Data.Char (isDigit)

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
  first  <- generaSubst t1 t3
  second <- generaSubst (aplicaSubst first t2) (aplicaSubst first t4)
  return (second ++ first) -- Posem les més recents primer
generaSubst (TVar ('t':s1)) (TVar s2) = Right [(('t':s1), (TVar s2))]  
generaSubst (TVar s1) (TVar s2) = Right [(s2, (TVar s1))]  
generaSubst t (TVar s) = Right [(s, t)]
generaSubst (TVar s) t = Right [(s, t)]
generaSubst _ _ =  Right []

envInicial :: Context
envInicial = funcionsPredefinides ++ [("True", TBool), ("False", TBool)]

renameTVar :: Tipus -> Int -> Maybe Tipus
renameTVar (TFun t1 t2) n = do 
  r1 <- (renameTVar t1 n)
  r2 <- (renameTVar t2 n)
  return (TFun r1 r2)
renameTVar (TVar ('t':rest)) n = Just (TVar ('t':rest))  --la deixem igual si comença amb t, (meves)
renameTVar (TVar s) n = Just (TVar (s++(show n)))
renameTVar t _ = Nothing

infereix :: Context -> Expr -> Int -> Either String (Tipus, Subst, Int)
infereix _ (Val _) i = Right (TInt, [], i) 

infereix ctx (Var a) n = case lookup a ctx of
                  Nothing -> Left "Variable no trobada"
                  Just t -> do 
                    case renameTVar t n of
                      Just t1 -> Right (t1, [], (n+1))
                      Nothing -> Right (t, [], n)

infereix ctx (Lam "True" e) n = infereix ctx (Lam "False" e) n
infereix ctx (Lam "False" e) n = do
  t_s <- Right (TBool)                                    -- Tipus de variable de lambda (b)
  (t1, s1, n1) <- infereix ctx e n                        -- Tipus del cos de lambda (c)
  t_retorn <- Right (aplicaSubst s1 (TFun t_s t1))        -- a = b -> c
  return (t_retorn, s1, n1)

infereix ctx (Lam s e) n = do
  case all isDigit s of
    True -> (do  
      t_s <- Right (TInt)                                     -- Tipus de variable de lambda (b)
      (t1, s1, n1) <- infereix ctx e n                        -- Tipus del cos de lambda (c)
      t_retorn <- Right (aplicaSubst s1 (TFun t_s t1))        -- a = b -> c
      return (t_retorn, s1, n1)
      )
    False -> (do  
      t_s <- Right (TVar ("t"++ show n ))                     -- Tipus de variable de lambda (b)
      (t1, s1, n1) <- infereix ((s,t_s):ctx) e (n+1)          -- Tipus del cos de lambda (c)
      t_retorn <- Right (aplicaSubst s1 (TFun t_s t1))        -- a = b -> c
      return (t_retorn, s1, n1)
      )

infereix ctx (App e1 e2) n = do
  (t1, s1, n1) <- infereix ctx e1 n                       -- Tipus del cos de l'aplicació (b)
  (t2, s2, n2) <- infereix (aplicaSubstCtx s1 ctx) e2 n1  -- Tipus del cos de lambda (c)
  
  t3 <- Right (TVar ("t" ++ show n2))                     -- Tipus de l'aplicació (a)

  s3 <- generaSubst (aplicaSubst s2 t1) (TFun t2 t3)      -- b = c -> a

  
  return (aplicaSubst s3 t3, s3 ++ s2 ++ s1, n2 + 1)

infereix ctx (If e1 e2 e3) n = do
  (t1, s1, n1) <- infereix ctx e1 n
  sCond <- generaSubst t1 TBool
  let sAcc1 = sCond ++ s1

  (t2 ,s2, n2) <- infereix (aplicaSubstCtx sAcc1 ctx) e2 n1
  let sAcc2 = s2 ++ sAcc1

  (t3, s3, n3) <- infereix (aplicaSubstCtx sAcc2 ctx) e3 n2
  sUnif <- generaSubst t3 t2
  let sAcc3 = sUnif ++ sAcc2 

  -- Retornem el tipus real de la branca (t2) corregit amb les substitucions
  return (aplicaSubst sAcc3 t2, sAcc3, n3)

-- Inferència per a una definició potencialment recursiva
-- Context -> Nom de la funció -> Cos de la funció -> Comptador -> Resultat
infereixDefRec :: Context -> String -> Expr -> Int -> Either String (Tipus, Subst, Int)
infereixDefRec ctx nomFunció cosExpr n = do
  -- 1. Creem una variable de tipus "fresca" assumint que és el tipus de la funció
  let t_suposat = TVar ("t" ++ show n)
  
  -- 2. Afegim aquesta hipòtesi al context i inferim el cos de la funció
  -- Ara, si el cos en fa ús, 'lookup' la trobarà com a (TVar "tX")
  (t_cos, s1, n1) <- infereix ((nomFunció, t_suposat) : ctx) cosExpr (n + 1)
  
  -- 3. Unifiquem el tipus que havíem suposat (aplicant-li les substitucions acumulades)
  -- amb el tipus real que ens ha retornat el cos
  s2 <- generaSubst (aplicaSubst s1 t_suposat) t_cos
  
  -- 4. Ajuntem totes les substitucions i retornem el tipus final corregit
  let sTotal = s2 ++ s1
  return (aplicaSubst sTotal t_cos, sTotal, n1)
