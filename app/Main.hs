module Main where
import BallotParser
import Text.Parsec
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            contents <- readFile filename
            case parseBallot contents of
                Left e  -> putStrLn $ "Error: " ++ show e
                Right b -> print b
        _ -> putStrLn "Usage: ballot-parser <filename>"