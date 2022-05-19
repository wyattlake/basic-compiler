module Token (
    Token (..),
    TokenEnum (..),
    tokenFromSingleChar,
    tokenfromString,
    TokenStream,
    blankRegex,
    nullToken,
) where

import Core (Pos (..))
import Text.Regex.Posix ((=~))

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
    | Null
    | Ident
    | IntLiteral
    | Negate
    | BitwiseComplement
    | LogicalNegate
    | TokenValidator [TokenEnum] -- Used for parsing only
    deriving (Show, Eq)

data Token = Token
    { tokenType :: TokenEnum
    , pos :: Pos
    , len :: Int
    , tokenData :: Maybe String
    }
    deriving (Show, Eq)

nullToken = Token Null (Pos 0 0) 0

buildToken :: TokenEnum -> Pos -> Int -> Token
buildToken token pos len = Token token (Pos (line pos) (column pos - len)) len Nothing

buildTokenOffset :: TokenEnum -> Pos -> Int -> Int -> Token
buildTokenOffset token pos len offset =
    Token token (Pos (line pos) (column pos - len + offset)) len Nothing

buildDataToken :: TokenEnum -> Pos -> Int -> String -> Token
buildDataToken token pos len str = Token token (Pos (line pos) (column pos - len)) len (Just str)

tokenFromSingleChar :: Char -> Pos -> Maybe Token
tokenFromSingleChar char pos = case char of
    '{' -> Just $ buildTokenOffset OpenBrace pos 1 1
    '}' -> Just $ buildTokenOffset CloseBrace pos 1 1
    '(' -> Just $ buildTokenOffset OpenParen pos 1 1
    ')' -> Just $ buildTokenOffset CloseParen pos 1 1
    ';' -> Just $ buildTokenOffset Semicolon pos 1 1
    '-' -> Just $ buildTokenOffset Negate pos 1 1
    '~' -> Just $ buildTokenOffset BitwiseComplement pos 1 1
    '!' -> Just $ buildTokenOffset LogicalNegate pos 1 1
    _ -> Nothing

tokenfromString :: String -> Pos -> Maybe Token
tokenfromString input pos
    | input == "int" = Just $ buildToken IntKeyword pos 3
    | input == "return" = Just $ buildToken ReturnKeyword pos 6
    | input =~ "^[a-zA-Z]\\w*" =
        Just $ buildDataToken Ident pos (length input) input
    | input =~ "^[0-9]+$" =
        Just $ buildDataToken IntLiteral pos (length input) input
    | otherwise = Nothing