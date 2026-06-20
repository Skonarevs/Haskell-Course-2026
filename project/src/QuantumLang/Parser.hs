module QuantumLang.Parser
  ( ParseError (..),
    parseProgram,
  )
where

import Control.Monad (ap)
import Data.Char (toLower)
import QuantumLang.Lexer
import QuantumLang.Types

many :: Parser a -> Parser [a]
many p = manyAcc []
  where
    manyAcc acc = do
      ok <- optional p
      case ok of
        Nothing -> pure (reverse acc)
        Just x -> manyAcc (x : acc)

optional :: Parser a -> Parser (Maybe a)
optional (Parser p) =
  Parser
    ( \ts ->
        case p ts of
          Left _ -> Right (Nothing, ts)
          Right (a, rest) -> Right (Just a, rest)
    )

data ParseError = ParseError
  { parseErrorLine :: Int,
    parseErrorColumn :: Int,
    parseErrorMessage :: String
  }
  deriving (Eq, Show)

newtype Parser a = Parser {runParser :: [Located] -> Either ParseError (a, [Located])}

instance Functor Parser where
  fmap f (Parser p) = Parser $ \ts -> fmap (first f) (p ts)
    where
      first g (a, rest) = (g a, rest)

instance Applicative Parser where
  pure a = Parser $ \ts -> Right (a, ts)
  (<*>) = ap

instance Monad Parser where
  Parser p >>= f = Parser $ \ts ->
    case p ts of
      Left err -> Left err
      Right (a, rest) -> runParser (f a) rest

parseProgram :: String -> Either ParseError Program
parseProgram src =
  case lexProgram src of
    Left (LexError line col msg) ->
      Left (ParseError line col ("lexical error: " ++ msg))
    Right tokens ->
      case runParser (program <* eof) tokens of
        Left err -> Left err
        Right (prog, _) -> Right prog

program :: Parser Program
program = Program <$> many statement

statement :: Parser Statement
statement = do
  tok <- peekToken
  case fmap locToken tok of
    Just TokInit -> parseInit
    Just TokApply -> parseApply
    Just TokMeasure -> parseMeasure
    Just TokPrint -> parsePrint
    Just TokIf -> parseIf
    Just TokRepeat -> parseRepeat
    Just TokEnd -> failHere "unexpected 'end' outside of a block"
    Just TokEOF -> failHere "unexpected end of file"
    _ -> failHere "expected statement"

parseInit :: Parser Statement
parseInit = do
  expect TokInit
  n <- expectInt
  pure (Init n)

parseApply :: Parser Statement
parseApply = do
  expect TokApply
  gate <- parseGate
  qubits <- parseQubitList
  pure (Apply gate qubits)

parseGate :: Parser Gate
parseGate = do
  name <- expectIdent
  case map toLower name of
    "h" -> pure H
    "x" -> pure X
    "y" -> pure Y
    "z" -> pure Z
    "cnot" -> pure CNOT
    "phase" -> Phase <$> expectDouble
    _ -> failHere ("unknown gate: " ++ name)

parseQubitList :: Parser [Int]
parseQubitList = do
  expect TokLBracket
  first <- expectInt
  rest <- many (expect TokComma >> expectInt)
  expect TokRBracket
  pure (first : rest)

parseMeasure :: Parser Statement
parseMeasure = do
  expect TokMeasure
  qubit <- expectInt
  expect TokArrow
  var <- expectIdent
  pure (Measure qubit var)

parsePrint :: Parser Statement
parsePrint = do
  expect TokPrint
  var <- expectIdent
  pure (Print var)

parseIf :: Parser Statement
parseIf = do
  expect TokIf
  var <- expectIdent
  body <- parseBlock
  pure (If var body)

parseRepeat :: Parser Statement
parseRepeat = do
  expect TokRepeat
  n <- expectInt
  body <- parseBlock
  pure (Repeat n body)

parseBlock :: Parser [Statement]
parseBlock = parseUntilEnd []

parseUntilEnd :: [Statement] -> Parser [Statement]
parseUntilEnd acc = do
  tok <- peekToken
  case locToken <$> tok of
    Just TokEnd -> expect TokEnd >> pure (reverse acc)
    Just TokEOF -> failHere "expected 'end' before end of file"
    _ -> do
      stmt <- statement
      parseUntilEnd (stmt : acc)

peekToken :: Parser (Maybe Located)
peekToken = Parser $ \ts -> Right (listToMaybe ts, ts)

expect :: Token -> Parser ()
expect tok = do
  Located line col actual <- anyToken
  if actual == tok
    then pure ()
    else parseError line col ("expected " ++ show tok ++ ", got " ++ show actual)

expectInt :: Parser Int
expectInt = do
  Located line col tok <- anyToken
  case tok of
    TokInt n -> pure n
    _ -> parseError line col ("expected integer, got " ++ show tok)

expectDouble :: Parser Double
expectDouble = do
  Located line col tok <- anyToken
  case tok of
    TokDouble d -> pure d
    TokInt n -> pure (fromIntegral n)
    _ -> parseError line col ("expected number, got " ++ show tok)

expectIdent :: Parser String
expectIdent = do
  Located line col tok <- anyToken
  case tok of
    TokIdent s -> pure s
    _ -> parseError line col ("expected identifier, got " ++ show tok)

anyToken :: Parser Located
anyToken =
  Parser
    ( \ts ->
        case ts of
          [] -> Left (ParseError 0 0 "unexpected end of input")
          t : rest -> Right (t, rest)
    )

failHere :: String -> Parser a
failHere msg =
  Parser
    ( \ts ->
        case ts of
          [] -> Left (ParseError 0 0 msg)
          Located line col _ : _ -> Left (ParseError line col msg)
    )

parseError :: Int -> Int -> String -> Parser a
parseError line col msg = Parser $ \_ -> Left (ParseError line col msg)

eof :: Parser ()
eof = do
  tok <- peekToken >>= maybe (failHere "unexpected end of input") pure
  case locToken tok of
    TokEOF -> anyToken >> pure ()
    _ -> failHere "expected end of file"

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x : _) = Just x
