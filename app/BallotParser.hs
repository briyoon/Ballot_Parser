{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module BallotParser where

import Text.Parsec
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Number
import Data.Char (digitToInt)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON, encode, toJSON, object, KeyValue ((.=)), Value, pairs)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson.Types (toEncoding)

-- Data structures
data Ballot = Ballot {
    header :: Header,
    sections :: [Section]
} deriving (Show, Eq, Generic)

data Header = Header {
    title :: String,
    date :: String,
    instructions :: String
} deriving (Show, Eq, Generic)

data Section = Section {
    sectionName :: String,
    items :: [Item]
} deriving (Show, Eq, Generic)

data Item = Contest ContestData
          | Proposition PropositionData
          | RankedChoice RankedChoiceData
          | Approval ApprovalData
    deriving (Show, Eq, Generic)

data ContestData = ContestData {
    contestName :: String,
    contestVoteFor :: Int,
    contestChoices :: [Candidate],
    contestWriteIn :: Bool
} deriving (Show, Eq, Generic)

data PropositionData = PropositionData {
    propName :: String,
    propDescription :: String,
    propChoices :: [Option]
} deriving (Show, Eq, Generic)

data RankedChoiceData = RankedChoiceData {
    rankedChoiceName :: String,
    rankedChoiceVoteFor :: Int,
    rankedChoices :: [Candidate],
    rankedChoiceWriteIn :: Bool
} deriving (Show, Eq, Generic)

data ApprovalData = ApprovalData {
    approvalName :: String,
    approvalChoices :: [Choice]
} deriving (Show, Eq, Generic)

data Choice = CandidateChoice Candidate
            | OptionChoice Option
    deriving (Show, Eq, Generic)

data Candidate = Candidate {
    name :: String,
    party :: String
} deriving (Show, Eq, Generic)

newtype Option = Option String
    deriving (Show, Eq, Generic)

-- Instances for JSON encoding
instance ToJSON Ballot
instance FromJSON Ballot
instance ToJSON Header
instance FromJSON Header
instance ToJSON Section
instance FromJSON Section
instance ToJSON Item where
    toJSON (Contest contest) = object ["contest" .= contest]
    toJSON (Proposition proposition) = object ["proposition" .= proposition]
    toJSON (RankedChoice rankedChoice) = object ["rankedChoice" .= rankedChoice]
    toJSON (Approval approval) = object ["approval" .= approval]
instance FromJSON Item
instance ToJSON ContestData
instance FromJSON ContestData
instance ToJSON PropositionData
instance FromJSON PropositionData
instance ToJSON RankedChoiceData
instance FromJSON RankedChoiceData
instance ToJSON ApprovalData
instance FromJSON ApprovalData
instance ToJSON Choice where
    -- toJSON (CandidateChoice candidate) = object ["candidate" .= candidate]
    toJSON (CandidateChoice candidate) = object ["candidate" .= object ["name" .= name candidate, "party" .= party candidate]]
    toJSON (OptionChoice option) = object ["option" .= option]
instance FromJSON Choice
instance ToJSON Candidate where
    toJSON (Candidate name party) = object ["candidate" .= object ["name" .= name, "party" .= party]]
    -- toJSON (Candidate name party) = object ["name" .= name, "party" .= party]
instance FromJSON Candidate
instance ToJSON Option where
    toJSON (Option option) = object ["option" .= option]
instance FromJSON Option

-- Parsers
ballotParser :: Parser Ballot
ballotParser = do
    lexeme $ string "Ballot:"
    header <- lexeme headerParser
    sections <- many1 $ lexeme sectionParser
    return $ Ballot header sections

headerParser :: Parser Header
headerParser = do
    lexeme $ string "Header:"
    title <- lexeme quotedString
    lexeme $ string "Date:"
    date <- lexeme dateParser
    lexeme $ string "Instructions:"
    instructions <- lexeme quotedString
    return $ Header title date instructions

sectionParser :: Parser Section
sectionParser = do
    lexeme $ string "Section:"
    sectionName <- lexeme quotedString
    items <- many1 $ lexeme itemParser
    return $ Section sectionName items

itemParser :: Parser Item
itemParser = try contestParser
          <|> try propositionParser
          <|> try rankedChoiceParser
          <|> approvalParser

contestParser :: Parser Item
contestParser = do
    lexeme $ string "Contest:"
    contestName <- lexeme quotedString
    voteFor <- lexeme voteForParser
    candidates <- many1 $ lexeme $ try candidateParser
    writeIn <- option False $ lexeme $ try writeInParser
    return $ Contest $ ContestData contestName voteFor candidates writeIn

candidateParser :: Parser Candidate
candidateParser = do
    lexeme $ string "Candidate:"
    lexeme $ string "Name:"
    name <- lexeme quotedString
    lexeme $ string "Party:"
    party <- lexeme quotedString
    return $ Candidate name party

propositionParser :: Parser Item
propositionParser = do
    lexeme $ string "Proposition:"
    propName <- lexeme quotedString
    lexeme $ string "Description:"
    propDescription <- lexeme quotedString
    options <- many1 $ lexeme $ try optionParser
    return $ Proposition $ PropositionData propName propDescription options

rankedChoiceParser :: Parser Item
rankedChoiceParser = do
    lexeme $ string "Ranked Choice:"
    rankedChoiceName <- lexeme quotedString
    voteFor <- lexeme voteForParser
    rankedChoices <- many1 $ lexeme $ try candidateParser
    writeIn <- option False $ lexeme $ try writeInParser
    return $ RankedChoice $ RankedChoiceData rankedChoiceName voteFor rankedChoices writeIn

approvalParser :: Parser Item
approvalParser = do
    lexeme $ string "Approval:"
    approvalName <- lexeme quotedString
    approvalChoices <- many1 $ lexeme $ try ((CandidateChoice <$> candidateParser) <|> (OptionChoice <$> optionParser))
    return $ Approval (ApprovalData approvalName approvalChoices)

dateParser :: Parser String
dateParser = do
    year <- count 4 digit
    char '-'
    month <- count 2 digit
    char '-'
    day <- count 2 digit
    return $ year ++ "-" ++ month ++ "-" ++ day

voteForParser :: Parser Int
voteForParser = do
    lexeme $ string "Vote-for:"
    voteFor <- lexeme $ many1 digit
    return $ numberValue 10 voteFor

writeInParser :: Parser Bool
writeInParser = do
    lexeme $ string "Write-in:"
    writeIn <- lexeme $ choice [string "true", string "false"]
    return $ writeIn == "true"

optionParser :: Parser Option
optionParser = do
    lexeme $ string "Option:"
    option <- lexeme quotedString
    return $ Option option

-- Helper parser for quoted strings
quotedString :: Parser String
quotedString = char '"' *> many (noneOf "\"") <* char '"'

lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

-- Function to parse a ballot
parseBallot :: String -> Either ParseError Ballot
parseBallot = parse ballotParser ""

ballotToJson :: Ballot -> String
ballotToJson ballot = BL.unpack $ encode ballot