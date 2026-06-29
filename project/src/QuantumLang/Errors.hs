module QuantumLang.Errors
  ( formatParseError,
    formatLexError,
  )
where

import QuantumLang.Lexer (LexError (..))
import QuantumLang.Parser (ParseError (..))

formatParseError :: String -> ParseError -> String
formatParseError source (ParseError line col msg) =
  unlines (header : contextLines)
  where
    header = "  line " ++ show line ++ ", column " ++ show col ++ ": " ++ msg
    contextLines =
      case sourceLine source line of
        Nothing -> []
        Just txt ->
          [ "  | " ++ txt
          , "  | " ++ replicate (col + 2) ' ' ++ "^"
          ]

formatLexError :: String -> LexError -> String
formatLexError source (LexError line col msg) =
  formatParseError source (ParseError line col ("lexical error: " ++ msg))

sourceLine :: String -> Int -> Maybe String
sourceLine source n = go 1 (lines source)
  where
    go _ [] = Nothing
    go current (line : rest)
      | current == n = Just line
      | otherwise = go (current + 1) rest
