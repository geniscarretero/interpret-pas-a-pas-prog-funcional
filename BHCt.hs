import System.Console.Haskeline
import HSParser
import HSTipus
import HSInferenciaTipus
import Control.Applicative (Alternative (empty, (<|>)))
import Data.List (isPrefixOf)
import Data.Char (isSpace)

data Mode = Ast | Type | Eval

esComentari :: String -> Bool
esComentari l = "--" `isPrefixOf` (dropWhile isSpace l)

output :: Maybe String -> Mode -> Maybe String
output Nothing _ = Nothing
output (Just ":quit") _ = Nothing
output (Just (':':'t':' ':input)) Ast = output (Just input) Type
output (Just input) m = (do
  let l = parse expr input
  case l of
    [] -> if all isSpace input || esComentari input
         then return ""
         else return ("Error:\n" ++ input ++ "\n^--- El parser ha parat aquí")
    [(res, "")] -> (do
        -- Cas que és una expressió aparentment vàlida
        case m of
          Ast -> return (show res)
          Type -> (do
            case infereix envInicial res 1 of
              Left text -> return text
              Right (t, _, _) -> return (showTipus t)
            )
        )
    [(res, rest)] -> (do 
        let pos = length input - length rest
        return ("Error:\n"++input++"\n"++(replicate pos ' ' ++ "^--- El parser ha parat aquí"))
      )
    )

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
        minput <- getInputLine "bhct> "
        let result = output minput Ast       -- De moment el mode per defecte és Ast però serà Eval
        case result of 
          Nothing -> return ()
          Just o -> (do
            outputStrLn o
            loop
            )
