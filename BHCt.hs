import System.Console.Haskeline
import HSParser
import HSTipus
import HSInferenciaTipus
import Control.Applicative (Alternative (empty, (<|>)))

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
        minput <- getInputLine "bhct> "
        case minput of
            Nothing -> return () -- Sortir amb Ctrl+D
            Just ":quit" -> return ()
            Just input -> do
                case parse expr input of
                    [] -> (do
                        outputStrLn $ "Error:"
                        outputStrLn $ input
                        outputStrLn $ "^--- El parser ha parat aquí"
                        )
                    [(res, "")] -> (do
                        outputStrLn $ show res
                        case infereix envInicial res 0 of
                          Left text -> outputStrLn $ text
                          Right (t, _, _) -> outputStrLn $ show $showTipus t 
                        )
                    [(res, rest)] -> do
                        outputStrLn $ "Error: "
                    
                        -- Si falla, busquem el punt de ruptura
                        let pos = length input - length rest
                        outputStrLn $ input
                        outputStrLn $ replicate pos ' ' ++ "^--- El parser ha parat aquí"



                loop
