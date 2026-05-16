module HSFuncs where
import HSTipus


funcionsPredefinides :: [(String, Tipus)] 
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
                ("not", TFun TBool TBool),
                ("id", TFun (TVar "a") (TVar "a")),
                ("const", TFun (TVar "a") (TFun (TVar "b") (TVar "a"))),
                (".", TFun (TFun (TVar "b") (TVar "c")) (TFun (TFun (TVar "a") (TVar "b")) (TFun (TVar "a") (TVar "c"))))]