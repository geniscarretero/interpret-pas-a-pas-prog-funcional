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
output :: Maybe String -> Mode -> TypeEnv -> DefEnv -> Maybe (String, TypeEnv, DefEnv)
output Nothing _ _ _ = Nothing
output (Just ":quit") _ _ _= Nothing
output (Just (':':'t':' ':input)) Eval typeEnv defEnv = output (Just input) Type typeEnv defEnv 
output (Just (':':'a':' ':input)) Eval typeEnv defEnv = output (Just input) Ast typeEnv defEnv
output (Just input) m typeEnv defEnv =
  case m of
    Ast -> 
      case parse expr input of
        [] -> if all isSpace input || esComentari input
            then return ("", typeEnv, defEnv)
            else return (("Error:\n"++input++"\n^--- El parser ha parat aquí"), typeEnv, defEnv)
        [(res, "")] -> return ((show res), typeEnv, defEnv)
        [(res, rest)] -> (do 
            let pos = length input - length rest
            return ("Error:\n"++input++"\n"++replicate pos ' ' ++ "^--- El parser ha parat aquí", typeEnv, defEnv)
            )
    Type ->
      case parse expr input of
        [] -> if all isSpace input || esComentari input
            then return ("", typeEnv, defEnv)
            else return ("Error:\n" ++ input ++ "\n^--- El parser ha parat aquí", typeEnv, defEnv)
        [(res, "")] ->
            case infereix (HM.toList typeEnv) res 1 of
              Left text -> return (text, typeEnv, defEnv)
              Right (t, _, _) -> return ((showTipus t), typeEnv, defEnv)
        [(res, rest)] -> (do 
            let pos = length input - length rest
            return ("Error:\n"++input++"\n"++replicate pos ' ' ++ "^--- El parser ha parat aquí", typeEnv, defEnv)
            )
    Eval -> 
      case parse line input of
        [] -> if all isSpace input || esComentari input
            then return ("", typeEnv, defEnv)
            else return ("Error:\n" ++ input ++ "\n^--- El parser ha parat aquí", typeEnv, defEnv)
        [(res, "")] -> 
            case res of
              Left e -> case infereix (HM.toList typeEnv) e 1 of
                Left text -> return (text, typeEnv, defEnv) 
                Right _ -> (do
                  let (a, hs) = ast2graph e (0, IM.empty) []  
                
                  case debugEvalLoop (a,hs)  (-1) typeEnv defEnv of
                    Left text -> return (text, typeEnv, defEnv)
                    --Right (a1,hs1) -> return ((show (a1,hs1)), typeEnv, defEnv)
                    Right (a1,hs1) -> return ((hsprint (a1,hs1) False), typeEnv, defEnv)
                  )
              Right (Bind str e) -> 
                case infereixDefRec (HM.toList typeEnv) str e 1 of
                  Left text -> return (text, typeEnv, defEnv)
                  --Right (t, _, _) -> return ("typeEnv:\n"++show (HM.insert str t typeEnv)++ "\ndefEnv\n" ++ show (HM.insert str (FuncDef e) defEnv) ++ "\n" , (HM.insert str t typeEnv), (HM.insert str (FuncDef e) defEnv) )
                  Right (t, _, _) -> return ("" , (HM.insert str t typeEnv), (HM.insert str (FuncDef e) defEnv) )
        [(res, rest)] -> (do 
            let pos = length input - length rest
            return ("Error:\n"++input++"\n"++replicate pos ' ' ++ "^--- El parser ha parat aquí", typeEnv, defEnv)
            )

-- interactiu
main :: IO ()
main = runInputT defaultSettings (loop preludeTypeEnv preludeDefEnv)
  where
    loop ::TypeEnv->DefEnv-> InputT IO ()
    loop typeEnv defEnv = do
        minput <- getInputLine "bhct> "
        let result = output minput Eval typeEnv defEnv 
        case result of 
          Nothing -> return ()
          Just (o, te, de) -> (do
            outputStrLn o
            loop te de
            )
