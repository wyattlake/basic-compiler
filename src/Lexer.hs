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

lex :: String -> String -> Either LexError TokenStream
lex "" string = if string == ""
  then Right []
  else case tokenfromString string of
    Just token -> Right [token]
    Nothing    -> Left $ LexError (Pos 0 0) string
lex (x : xs) ""
  | [x] =~ blankRegex = Lexer.lex xs ""
  | otherwise = case tokenFromSingleChar x of
    Just token -> (:) token `fmap` Lexer.lex xs ""
    Nothing    -> Lexer.lex xs [x]
lex (x : xs) string = if isJust charToken || [x] =~ blankRegex
  then case tokenfromString string of
    Just stringToken -> case charToken of
      Just charToken ->
        (:) stringToken `fmap` (:) charToken `fmap` Lexer.lex xs ""
      Nothing -> (:) stringToken `fmap` Lexer.lex xs ""
    Nothing -> Left $ LexError (Pos 0 0) string
  else Lexer.lex xs $ string ++ [x]
  where charToken = tokenFromSingleChar x
