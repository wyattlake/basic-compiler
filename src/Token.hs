module Token
    ( Token(..)
    , tokenFromSingleChar
    , tokenfromString
    , TokenStream
    , blankRegex
    ) where

import           Text.Regex.Posix               ( (=~) )

type TokenStream = [Token]

blankRegex = "[ \n\t\r]"

data Token
    = OpenBrace
    | CloseBrace
    | OpenParen
    | CloseParen
    | Semicolon
    | IntKeyword
    | ReturnKeyword
    | Ident String
    | IntLiteral String
    deriving (Show)

tokenFromSingleChar :: Char -> Maybe Token
tokenFromSingleChar char = case char of
    '{' -> Just OpenBrace
    '}' -> Just CloseBrace
    '(' -> Just OpenParen
    ')' -> Just CloseParen
    ';' -> Just Semicolon
    _   -> Nothing

tokenfromString :: String -> Maybe Token
tokenfromString input | input == "int"           = Just IntKeyword
                      | input == "return"        = Just ReturnKeyword
                      | input =~ "^[a-zA-Z]\\w*" = Just $ Ident input
                      | input =~ "^[0-9]+$"      = Just $ IntLiteral input
                      | otherwise                = Nothing
