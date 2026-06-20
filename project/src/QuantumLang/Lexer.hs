module QuantumLang.Lexer
  ( Token (..),
    Located (..),
    LexError (..),
    lexProgram,
  )
where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace, toLower)

data Token
  = TokInit
  | TokApply
  | TokMeasure
  | TokIf
  | TokRepeat
  | TokPrint
  | TokEnd
  | TokArrow
  | TokLBracket
  | TokRBracket
  | TokComma
  | TokInt Int
  | TokDouble Double
  | TokIdent String
  | TokEOF
  deriving (Eq, Show)

data Located = Located
  { locLine :: !Int,
    locColumn :: !Int,
    locToken :: !Token
  }
  deriving (Eq, Show)

data LexError = LexError
  { lexErrorLine :: Int,
    lexErrorColumn :: Int,
    lexErrorMessage :: String
  }
  deriving (Eq, Show)

lexProgram :: String -> Either LexError [Located]
lexProgram src = go src 1 1 []
  where
    go [] line col acc = Right (reverse (Located line col TokEOF : acc))
    go (c : cs) line col acc
      | c == '\n' = go cs (line + 1) 1 acc
      | isSpace c = go cs line (col + 1) acc
      | c == '/' && take 2 (c : cs) == "//" =
          go (dropWhile (/= '\n') cs) line col acc
      | c == '-' && take 2 (c : cs) == "->" =
          go (drop 1 cs) line (col + 2) (Located line col TokArrow : acc)
      | c == '[' =
          go cs line (col + 1) (Located line col TokLBracket : acc)
      | c == ']' =
          go cs line (col + 1) (Located line col TokRBracket : acc)
      | c == ',' =
          go cs line (col + 1) (Located line col TokComma : acc)
      | isDigit c || (c == '.' && maybe False isDigit (listToMaybe cs)) =
          let (num, rest) = spanNumber (c : cs)
           in case parseNumber num of
                Left msg -> Left (LexError line col msg)
                Right tok ->
                  go rest line (col + length num) (Located line col tok : acc)
      | isAlpha c || c == '_' =
          let (word, rest) = span isIdentChar (c : cs)
              len = length word
           in go rest line (col + len) (Located line col (keywordOrIdent word) : acc)
      | otherwise =
          Left (LexError line col ("unexpected character: " ++ [c]))

    isIdentChar c = isAlphaNum c || c == '_'

    keywordOrIdent word =
      case map toLower word of
        "init" -> TokInit
        "apply" -> TokApply
        "measure" -> TokMeasure
        "if" -> TokIf
        "repeat" -> TokRepeat
        "print" -> TokPrint
        "end" -> TokEnd
        _ -> TokIdent word

    spanNumber s =
      let (whole, afterWhole) = span isDigit s
       in case afterWhole of
            '.' : rest ->
              let (frac, afterFrac) = span isDigit rest
               in (whole ++ '.' : frac, afterFrac)
            _ -> (whole, afterWhole)

    parseNumber num
      | null num = Left "empty number"
      | '.' `elem` num =
          case reads num :: [(Double, String)] of
            [(d, "")] -> Right (TokDouble d)
            _ -> Left ("invalid number: " ++ num)
      | otherwise =
          case reads num :: [(Int, String)] of
            [(n, "")] -> Right (TokInt n)
            _ -> Left ("invalid number: " ++ num)

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x : _) = Just x
