module BallotParser where

-- import Text.Parsec
-- import Text.Parsec.String

-- data Ballot = Ballot Header [Section]
--   deriving Show

-- data Header = Header String String String
--   deriving Show

-- data Section = Section String [Item]
--   deriving Show

-- data Item = ContestItem Contest
--           | PropositionItem Proposition
--           | RankedChoiceItem RankedChoice
--           | ApprovalItem Approval
--   deriving Show

-- data Contest = Contest String [Candidate] Bool
--   deriving Show

-- data Candidate = Candidate String String
--   deriving Show

-- data Proposition = Proposition String String [Option]
--   deriving Show

-- data RankedChoice = RankedChoice String [String]
--   deriving Show

-- data Approval = Approval String [String]
--   deriving Show

-- data Option = Option String
--   deriving Show

-- optionalNewlines :: Parser ()
-- optionalNewlines = skipMany newline

-- parseBallot :: Parser Ballot
-- parseBallot = do
--   optionalNewlines
--   header <- parseHeader
--   optionalNewlines
--   sections <- many1 parseSection
--   return $ Ballot header sections

-- parseHeader :: Parser Header
-- parseHeader = do
--   _ <- string "Election: "
--   title <- manyTill anyChar newline
--   date <- parseDate
--   instructions <- parseInstructions
--   optionalNewlines
--   return $ Header title date instructions

-- parseDate :: Parser String
-- parseDate = do
--   year <- count 4 digit
--   _ <- char '-'
--   month <- count 2 digit
--   _ <- char '-'
--   day <- count 2 digit
--   _ <- newline
--   return $ year ++ "-" ++ month ++ "-" ++ day

-- parseInstructions :: Parser String
-- parseInstructions = do
--   _ <- string "Instructions: "
--   manyTill anyChar newline

-- parseSection :: Parser Section
-- parseSection = do
--   _ <- string "Section: "
--   level <- manyTill anyChar newline
--   optionalNewlines
--   items <- many1 parseItem
--   optionalNewlines
--   return $ Section level items

-- parseItem :: Parser Item
-- parseItem = do
--   optionalNewlines
--   choice [try $ fmap ContestItem parseContest,
--           fmap PropositionItem parseProposition,
--           fmap RankedChoiceItem parseRankedChoice,
--           fmap ApprovalItem parseApproval]

-- parseContest :: Parser Contest
-- parseContest = do
--   _ <- string "Contest: "
--   name <- manyTill anyChar newline
--   optionalNewlines
--   candidates <- many1 parseCandidate
--   optionalNewlines
--   writeIn <- option False (string "Write-in: " >> fmap readBool (manyTill anyChar newline))
--   optionalNewlines
--   return $ Contest name candidates writeIn
--   where
--     readBool "true" = True
--     readBool _ = False

-- parseCandidate :: Parser Candidate
-- parseCandidate = do
--   optionalNewlines
--   _ <- string "Candidate: "
--   name <- manyTill anyChar (char ',')
--   _ <- space
--   party <- manyTill anyChar newline
--   return $ Candidate name party

-- parseProposition :: Parser Proposition
-- parseProposition = do
--   optionalNewlines
--   _ <- string "Proposition: "
--   propName <- manyTill anyChar newline
--   propDesc <- manyTill anyChar newline
--   options <- many1 parseOption
--   return $ Proposition propName propDesc options

-- parseRankedChoice :: Parser RankedChoice
-- parseRankedChoice = do
--   optionalNewlines
--   _ <- string "Ranked Choice: "
--   name <- manyTill anyChar newline
--   options <- many1 parseOptionName
--   return $ RankedChoice name options

-- parseApproval :: Parser Approval
-- parseApproval = do
--   optionalNewlines
--   _ <- string "Approval: "
--   name <- manyTill anyChar newline
--   options <- many1 parseOptionName
--   return $ Approval name options

-- parseOption :: Parser Option
-- parseOption = do
--   optionalNewlines
--   _ <- string "Option: "
--   optionText <- manyTill anyChar newline
--   return $ Option optionText

-- parseOptionName :: Parser String
-- parseOptionName = do
--   optionalNewlines
--   _ <- try (string "Candidate: ") <|> string "Option: "
--   manyTill anyChar newline
import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Char (isDigit, isLetter)

data Ballot = Ballot Header [Section] deriving Show
data Header = Header Title Date Instructions deriving Show
data Section = Section SectionName [Item] deriving Show
data Item = Contest ContestName [Candidate] (Maybe Bool)
          | Proposition PropName PropDesc [Option]
          | RankedChoice RankedChoiceName [Candidate] | [Option]
          | Approval ApprovalName [Candidate] | [Option]
          deriving Show
data Candidate = Candidate CandidateName Party

type Title = String
type Date = String
type Instructions = String

type SectionName = String
type ContestName = String

type CandidateName = String
type Party = String

type PropName = String
type PropDesc = String
type Option = String

type RankedChoiceName = String

type ApprovalName = String

parseBallot :: Parser Ballot
parseBallot = do
  header <- parseHeader
  _ <- newline
  sections <- many1 (parseSection <* newline)
  return $ Ballot header sections

parseHeader :: Parser Header
parseHeader = do
  _ <- string "Election: "
  title <- parseString
  date <- parseDate
  instructions <- many (noneOf "\n")
  return $ Header title date instructions

parseString :: Parser String
parseString = manyTill anyChar newline

parseDate :: Parser String
parseDate = do
  year <- count 4 digit
  _ <- char '-'
  month <- count 2 digit
  _ <- char '-'
  day <- count 2 digit
  _ <- newline
  return $ year ++ "-" ++ month ++ "-" ++ day

parseSection :: Parser Section
parseSection = do
  _ <- string "Section: "
  sectionLevel <- parseString
  _ <- newline
  items <- many1 (parseItem <* newline)
  return $ Section sectionLevel items

parseItem :: Parser Item
parseItem = try parseContest
        <|> try parseProposition
        <|> try parseRankedChoice
        <|> parseApproval

parseContest :: Parser Item
parseContest = do
  _ <- string "Contest: "
  contestName <- parseString
  _ <- newline
  candidates <- many1 (parseCandidate <* newline)
  writeIn <- optionMaybe parseWriteIn
  return $ Contest contestName candidates writeIn

parseCandidate :: Parser String
parseCandidate = do
  _ <- string "Candidate: "
  name <- parseString
  _ <- newline
  party <- parseString
  return $ name ++ ", " ++ party

parseWriteIn :: Parser Bool
parseWriteIn = do
  _ <- string "Write-In: "
  parseBool

parseBool :: Parser Bool
parseBool = (string "True" *> return True) <|> (string "False" *> return False)

parseProposition :: Parser Item
parseProposition = do
  _ <- string "Proposition: "
  propName <- parseString
  _ <- newline
  propDesc <- parseString
  _ <- newline
  options <- many1 (parseOption <* newline)
  return $ Proposition propName propDesc options

parseOption :: Parser String
parseOption = do
  _ <- string "Option: "
  optionText <- parseString
  return optionText

parseRankedChoice :: Parser Item
parseRankedChoice = do
  _ <- string "Ranked Choice: "
  rankedChoiceName <- parseString
  _ <- newline
  options <- many1 (parseOption <* newline)
  return $ RankedChoice rankedChoiceName options

parseApproval :: Parser Item
parseApproval = do
  _ <- string "Approval: "
  approvalName <- parseString
  _ <- newline
  options <- many1 (parseOption <* newline)
  return $ Approval approvalName options
