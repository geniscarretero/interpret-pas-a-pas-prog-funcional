import System.Console.Haskeline
import HSParser
import HSEval
import HSTipus
import HSDef
import HSInferenciaTipus
import Control.Applicative (Alternative (empty, (<|>)))
import qualified Data.IntMap as IM
import Data.List (isPrefixOf)
import Data.Char (isSpace)
import qualified Data.HashMap.Strict as HM

data Mode = Ast | Type | Eval

esComentari :: String -> Bool
esComentari l = "--" `isPrefixOf` (dropWhile isSpace l)
-- Interactiu:
output :: Maybe String -> Mode -> TypeEnv -> DefEnv -> Int -> Maybe (String, TypeEnv, DefEnv, Int)
output Nothing _ _ _ _ = Nothing
output (Just ":quit") _ _ _ _= Nothing
output (Just (':':'t':' ':input)) Eval typeEnv defEnv typeNum = output (Just input) Type typeEnv defEnv typeNum 
output (Just (':':'a':' ':input)) Eval typeEnv defEnv typeNum = output (Just input) Ast typeEnv defEnv typeNum
output (Just input) m typeEnv defEnv typeNum=
  case m of
    Ast -> 
      case parse expr input of
        [] -> if all isSpace input || esComentari input
            then return ("", typeEnv, defEnv, typeNum)
            else return (("Error:\n"++input++"\n^--- El parser ha parat aquí"), typeEnv, defEnv, typeNum)
        [(res, "")] -> return ((show res), typeEnv, defEnv, typeNum)
        [(res, rest)] -> (do 
            let pos = length input - length rest
            return ("Error:\n"++input++"\n"++replicate pos ' ' ++ "^--- El parser ha parat aquí", typeEnv, defEnv, typeNum)
            )
    Type ->
      case parse expr input of
        [] -> if all isSpace input || esComentari input
            then return ("", typeEnv, defEnv, typeNum)
            else return ("Error:\n" ++ input ++ "\n^--- El parser ha parat aquí", typeEnv, defEnv, typeNum)
        [(res, "")] ->
            case infereix (HM.toList typeEnv) (HM.toList typeEnv) res typeNum of
              Left text -> return (text, typeEnv, defEnv, typeNum)
              Right (t, _, newTypeNum) -> return ((showTipus t), typeEnv, defEnv, newTypeNum)
        [(res, rest)] -> (do 
            let pos = length input - length rest
            return ("Error:\n"++input++"\n"++replicate pos ' ' ++ "^--- El parser ha parat aquí", typeEnv, defEnv, typeNum)
            )
    Eval -> 
      case parse line input of
        [] -> if all isSpace input || esComentari input
            then return ("", typeEnv, defEnv, typeNum)
            else return ("Error:\n" ++ input ++ "\n^--- El parser ha parat aquí", typeEnv, defEnv, typeNum)
        [(res, "")] -> 
            case res of
              Left e -> case infereix (HM.toList typeEnv) (HM.toList typeEnv) e typeNum of
                Left text -> return (text, typeEnv, defEnv, typeNum) 
                Right (_, _, newTypeNum) -> 
                  case e of
                  (Var nom) -> 
                    (do
                      let (a, hs) = ast2graph e (0, IM.empty) []  
                    
                      case evalVar (a,hs) typeEnv defEnv [] of
                        Left text -> return (text, typeEnv, defEnv, newTypeNum)
                        --Right (a1,hs1) -> return ((show (a1,hs1)), typeEnv, defEnv)
                        Right ((a1,hs1), trace) -> return ((foldl (\x y -> x ++ y++ "\n") "" trace), typeEnv, (HM.insert nom (FuncDef (graph2ast (a1,hs1))) defEnv), newTypeNum)
                      )
                  _ ->
                    (do
                      let (a, hs) = ast2graph e (0, IM.empty) []  
                      --return ((show (a,hs) ++ show typeEnv), typeEnv, defEnv, newTypeNum) -- Esborrar
                    
                      case evalLoop (a,hs)  (-1) typeEnv defEnv [] of
                        Left text -> return (text, typeEnv, defEnv, newTypeNum)
                        --Right (a1,hs1) -> return ((show (a1,hs1)), typeEnv, defEnv)
                        Right ((a1,hs1), trace) -> return ((foldl (\x y -> x ++ y++ "\n") "" trace), typeEnv, defEnv, newTypeNum)
                      )
              Right (Bind str e) -> 
                case infereixDefRec (HM.toList typeEnv) (HM.toList typeEnv) str e typeNum of
                  Left text -> return (text, typeEnv, defEnv, typeNum)
                  --Right (t, _, newTypeNum) -> return ("typeEnv:\n"++show (HM.insert str t typeEnv)++ "\ndefEnv\n" ++ show (HM.insert str (FuncDef e) defEnv) ++ "\n" , (HM.insert str t typeEnv), (HM.insert str (FuncDef e) defEnv), newTypeNum )
                  Right (t, _, newTypeNum) -> return ("" , (HM.insert str t typeEnv), (HM.insert str (FuncDef e) defEnv), newTypeNum )
        [(res, rest)] -> (do 
            let pos = length input - length rest
            return ("Error:\n"++input++"\n"++replicate pos ' ' ++ "^--- El parser ha parat aquí", typeEnv, defEnv, typeNum)
            )

-- interactiu
main :: IO ()
main = runInputT defaultSettings (loop preludeTypeEnv preludeDefEnv 1)
  where
    loop ::TypeEnv->DefEnv->Int-> InputT IO ()
    loop typeEnv defEnv typeNum = do
        minput <- getInputLine "bhct> "
        let result = output minput Eval typeEnv defEnv typeNum
        case result of 
          Nothing -> return ()
          Just (o, te, de, tn) -> (do
            outputStrLn o
            loop te de tn
            )
