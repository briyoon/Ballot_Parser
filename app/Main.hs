module Main where
import BallotParser
import Text.Parsec
import System.Environment (getArgs)

main :: IO ()
main = do
  input <- getContents
  case parseBallot input of
    Left err -> print err
    Right ballot -> putStrLn $ ballotToJson ballot
