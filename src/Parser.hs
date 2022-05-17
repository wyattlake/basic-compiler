module Parser (ParseError (..), Expr (..), Statement (..), Program (..), parseProgram) where

import Control.Exception (evaluate)
import Control.Monad
import Data.Maybe
import Token (
    Token (tokenData, tokenType),
    TokenEnum (..),
    TokenStream,
    nullToken,
 )

newtype Expr = Expr Int deriving (Show)

newtype Statement = Return Expr deriving (Show)

data FnDec = FnDec
    { name :: String
    , body :: Statement
    }
    deriving (Show)

newtype Program = Program FnDec deriving (Show)

data ParseError = ParseError
    { token :: Maybe Token
    , message :: String
    }
    deriving (Show)

expectedError :: Token -> TokenEnum -> Either ParseError a
expectedError token tokenType = Left $ ParseError (Just token) ("Expected " ++ show tokenType)

nextToken :: TokenStream -> Either ParseError (Token, TokenStream)
nextToken [] = Left $ ParseError Nothing "Unexpected end of input"
nextToken (x : xs) = Right (x, xs)

nextTokens :: Int -> (TokenStream, TokenStream) -> Either ParseError (TokenStream, TokenStream)
nextTokens 0 (xs, ys) = Right (xs, ys)
nextTokens _ (_, []) = Left $ ParseError Nothing "Unexpected end of input"
nextTokens n (xs, y : ys) = nextTokens (n - 1) (xs, ys) >>= \(xs', ys') -> Right (xs ++ [y], ys')

checkTokens :: [TokenEnum] -> (TokenStream, TokenStream) -> Either ParseError (TokenStream, TokenStream)
checkTokens [] (dataTokens, remainingTokens) = Right (dataTokens, remainingTokens)
checkTokens (x : xs) (dataTokens, remainingTokens) = do
    (token, tokens) <- nextToken remainingTokens
    if tokenType token /= x
        then expectedError token x
        else
            if isJust $ tokenData token
                then checkTokens xs (dataTokens ++ [token], tokens)
                else checkTokens xs (dataTokens, tokens)

parseExpr :: TokenStream -> Either ParseError (Expr, TokenStream)
parseExpr tokens = do
    (token, tokens) <- nextToken tokens
    case tokenType token of
        IntLiteral -> return (Expr $ read $ fromJust $ tokenData token, tokens)
        _ -> expectedError token IntLiteral

parseStatement :: TokenStream -> Either ParseError (Statement, TokenStream)
parseStatement tokens = do
    (_, tokens) <- checkTokens [ReturnKeyword] ([], tokens)
    (expr, tokens) <- parseExpr tokens
    (_, tokens) <- checkTokens [Semicolon] ([], tokens)
    return (Return expr, tokens)

parseFnDec :: TokenStream -> Either ParseError (FnDec, TokenStream)
parseFnDec tokens = do
    (dataTokens, tokens) <- checkTokens [IntKeyword, Ident, OpenParen, CloseParen, OpenBrace] ([], tokens)
    (body, tokens) <- parseStatement tokens
    (_, tokens) <- checkTokens [CloseBrace] ([], tokens)
    return (FnDec (fromJust $ tokenData $ head dataTokens) body, tokens)

parseProgram :: TokenStream -> Either ParseError Program
parseProgram tokens = do
    (fnDec, tokens) <- parseFnDec tokens
    return $ Program fnDec