import System.Console.Haskeline
import HSParser
import HSTipus
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
                    [(res, "")] -> outputStrLn $ show res
                    [] -> (do
                        outputStrLn $ "Error:"
                        outputStrLn $ input
                        outputStrLn $ "^--- El parser ha parat aquí"
                        )
                    [(res, rest)] -> do
                        outputStrLn $ "Error: "
                    
                        -- Si falla, busquem el punt de ruptura
                        let pos = length input - length rest
                        outputStrLn $ input
                        outputStrLn $ replicate pos ' ' ++ "^--- El parser ha parat aquí"

                loop