module Lexer
  ( Lexer.lex
  , LexError(..)
  ) where

import           Core
import           Data.Maybe
import           Text.Regex.Posix               ( (=~) )
import           Token

data LexError = LexError
  { position :: Pos
  , message  :: String
  }
  deriving Show

throwLexError :: Pos -> String -> LexError
throwLexError pos string =
  LexError (Pos (line pos) (column pos - length string))
    $  "Invalid token: "
    ++ string

lex :: String -> String -> Pos -> Either LexError TokenStream
lex "" string pos = if string == ""
  then Right []
  else case tokenfromString string pos of
    Just token -> Right [token]
    Nothing    -> Left $ throwLexError pos string
lex (x : xs) "" pos
  | [x] =~ blankRegex = Lexer.lex xs "" $ nextPos pos x
  | otherwise = case tokenFromSingleChar x pos of
    Just token -> (:) token `fmap` Lexer.lex xs "" (nextPos pos x)
    Nothing    -> Lexer.lex xs [x] $ nextPos pos x
lex (x : xs) string pos = if isJust charToken || [x] =~ blankRegex
  then case tokenfromString string pos of
    Just stringToken -> case charToken of
      Just charToken -> (:) stringToken `fmap` (:) charToken `fmap` Lexer.lex
        xs
        ""
        (nextPos pos x)
      Nothing -> (:) stringToken `fmap` Lexer.lex xs "" (nextPos pos x)
    Nothing -> Left $ throwLexError pos string
  else Lexer.lex xs (string ++ [x]) $ nextPos pos x
  where charToken = tokenFromSingleChar x pos
