module Token
    ( Token(..)
    , tokenFromSingleChar
    , tokenfromString
    , TokenStream
    , blankRegex
    ) where

import           Core                           ( Pos(..) )
import           Text.Regex.Posix               ( (=~) )

type TokenStream = [Token]

blankRegex = "[ \n\t\r]"

data TokenEnum
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

data Token = Token
    { token :: TokenEnum
    , pos   :: Pos
    , len   :: Int
    }
    deriving Show

buildToken :: TokenEnum -> Pos -> Int -> Token
buildToken token pos len = Token token (Pos (line pos) (column pos - len)) len

buildTokenOffset :: TokenEnum -> Pos -> Int -> Int -> Token
buildTokenOffset token pos len offset =
    Token token (Pos (line pos) (column pos - len + offset)) len

tokenFromSingleChar :: Char -> Pos -> Maybe Token
tokenFromSingleChar char pos = case char of
    '{' -> Just $ buildTokenOffset OpenBrace pos 1 1
    '}' -> Just $ buildTokenOffset CloseBrace pos 1 1
    '(' -> Just $ buildTokenOffset OpenParen pos 1 1
    ')' -> Just $ buildTokenOffset CloseParen pos 1 1
    ';' -> Just $ buildTokenOffset Semicolon pos 1 1
    _   -> Nothing

tokenfromString :: String -> Pos -> Maybe Token
tokenfromString input pos
    | input == "int" = Just $ buildToken IntKeyword pos 3
    | input == "return" = Just $ buildToken ReturnKeyword pos 6
    | input =~ "^[a-zA-Z]\\w*" = Just $ buildToken (Ident input) pos $ length
        input
    | input =~ "^[0-9]+$" = Just $ buildToken (IntLiteral input) pos $ length
        input
    | otherwise = Nothing
