module Main where
import BallotParser
import Text.Parsec
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputFile] -> do
      input <- readFile inputFile
      case parse parseBallot "" input of
        Left e -> putStrLn $ "Error: " ++ show e
        Right ballot -> print ballot
    _ -> putStrLn "Usage: ballot-parser <input-file>"

-- main :: IO ()
-- main = do
--   let input = "Election: General Election 2023\n2023-11-08\nInstructions: To cast your vote, select your choice and confirm your selection.\n\nSection: National\nContest: President\nCandidate: John Smith, Democrat\nCandidate: Jane Doe, Republican\nWrite-in: true\n\nProposition: Prop 1 - Infrastructure Investment\nProp 1 aims to increase funding for infrastructure projects, including roads, bridges, and public transportation.\nOption: Yes\nOption: No\n\nRanked Choice: Best Ice Cream Flavor\nCandidate: Chocolate\nCandidate: Vanilla\nCandidate: Strawberry\n\nApproval: Select preferred board members\nCandidate: Alice Johnson, Independent\nCandidate: Bob Martinez, Independent\nCandidate: Charles Lee, Independent\n\nSection: State\nContest: Governor\nCandidate: Laura Brown, Democrat\nCandidate: Steven Johnson, Republican\n\nProposition: Prop 2 - Educational Funding\nProp 2 proposes an increase in educational funding for public schools, aiming to reduce class sizes and provide more resources for students.\nOption: Yes\nOption: No"
--   case parse parseBallot "" input of
--     Left e -> putStrLn $ "Error: " ++ show e
--     Right ballot -> print ballot