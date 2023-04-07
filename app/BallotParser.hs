module BallotParser where

import Text.Parsec
import Text.Parsec.String
import Data.Char (digitToInt)

-- Data structures
data Ballot = Ballot { header :: Header, sections :: [Section] }
            deriving (Show, Eq)
data Header = Header { title :: String, date :: String, instructions :: String }
            deriving (Show, Eq)
data Section = Section { sectionName :: String, items :: [Item] }
            deriving (Show, Eq)
data Item = Contest ContestData
          | Proposition PropositionData
          | RankedChoice RankedChoiceData
          | Approval ApprovalData
          deriving (Show, Eq)
data ContestData = ContestData { contestName :: String, candidates :: [Candidate], writeIn :: Bool }
                deriving (Show, Eq)
data PropositionData = PropositionData { propName :: String, propDescription :: String, options :: [Option] }
                deriving (Show, Eq)
data RankedChoiceData = RankedChoiceData { rankedChoiceName :: String, rankedChoices :: [RankedChoiceOption] }
                      deriving (Show, Eq)
data ApprovalData = ApprovalData { approvalName :: String, approvals :: [RankedChoiceOption] }
                deriving (Show, Eq)
data Candidate = Candidate { name :: String, party :: String }
                deriving (Show, Eq)
data RankedChoiceOption = RankedChoiceOption Option
                        | RankedChoiceCandidate Candidate
                        deriving (Show, Eq)
data ApprovalOption = ApprovalOption Option
                    | ApprovalCandidate Candidate
                    deriving (Show, Eq)
type Option = String

-- Parsers
ballotParser :: Parser Ballot
ballotParser = do
    header <- headerParser
    many newline
    sections <- many1 sectionParser
    return $ Ballot header sections

headerParser :: Parser Header
headerParser = do
    many newline
    string "Election: "
    title <- manyTill anyChar newline
    year <- count 4 digit
    char '-'
    month <- count 2 digit
    char '-'
    day <- count 2 digit
    skipMany1 newline
    instructions <- manyTill anyChar newline
    return $ Header title (year ++ "-" ++ month ++ "-" ++ day) instructions

sectionParser :: Parser Section
sectionParser = do
    many newline
    string "Section: "
    sectionName <- manyTill anyChar newline
    items <- many1 itemParser
    skipMany newline
    return $ Section sectionName items

itemParser :: Parser Item
itemParser = try contestParser
          <|> try propositionParser
          <|> try rankedChoiceParser
          <|> try approvalParser

contestParser :: Parser Item
contestParser = do
    many newline
    string "Contest: "
    contestName <- manyTill anyChar (try newline)
    candidates <- many1 candidateParser
    writeIn <- option False (try writeInParser)
    return $ Contest (ContestData contestName candidates writeIn)

candidateParser :: Parser Candidate
candidateParser = do
    many newline
    string "Candidate: "
    name <- manyTill anyChar (try (string ", "))
    party <- manyTill anyChar newline
    return $ Candidate name party

writeInParser :: Parser Bool
writeInParser = do
    many newline
    string "Write-in: "
    writeIn <- choice [string "true", string "false"]
    newline
    return $ writeIn == "true"

propositionParser :: Parser Item
propositionParser = do
    many newline
    string "Proposition: "
    propName <- manyTill anyChar newline
    propDescription <- manyTill anyChar newline
    options <- many1 optionParser
    return $ Proposition (PropositionData propName propDescription options)

rankedChoiceParser :: Parser Item
rankedChoiceParser = do
    many newline
    string "Ranked Choice: "
    rankedChoiceName <- manyTill anyChar newline
    rankedChoices <- many1 (try (RankedChoiceOption <$> optionParser) <|> (RankedChoiceCandidate <$> candidateParser))
    return $ RankedChoice (RankedChoiceData rankedChoiceName rankedChoices)

approvalParser :: Parser Item
approvalParser = do
    many newline
    string "Approval: "
    approvalName <- manyTill anyChar newline
    approvals <- many1 (try (RankedChoiceOption <$> optionParser) <|> (RankedChoiceCandidate <$> candidateParser))
    return $ Approval (ApprovalData approvalName approvals)

optionParser :: Parser Option
optionParser = do
    string "Option: "
    option <- manyTill anyChar newline
    return option

-- Function to parse a ballot
parseBallot :: String -> Either ParseError Ballot
parseBallot input = parse ballotParser "" input
