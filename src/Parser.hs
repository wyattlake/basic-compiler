{-# LANGUAGE ConstrainedClassMethods #-}

module Parser (ParseError (..), Expr (..), Statement (..), Program (..), FnDec (..), ASTNode (..), parseProgram) where

import Control.Exception (evaluate)
import Control.Monad
import Data.Maybe
import Token (
    Token (tokenData, tokenType),
    TokenEnum (..),
    TokenStream,
    nullToken,
 )

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

isValid :: TokenEnum -> TokenEnum -> Bool
isValid x y = case y of
    TokenValidator tokens -> x `elem` tokens
    _ -> x == y

isValidator :: TokenEnum -> Bool
isValidator x = case x of
    TokenValidator _ -> True
    _ -> False

checkTokens :: [TokenEnum] -> TokenStream -> Either ParseError TokenStream
checkTokens [] tokens = Right tokens
checkTokens (x : xs) tokens = do
    (token, tokens) <- nextToken tokens
    if isValid (tokenType token) x
        then checkTokens xs tokens
        else expectedError token x

checkDataTokens :: [TokenEnum] -> (TokenStream, TokenStream) -> Either ParseError (TokenStream, TokenStream)
checkDataTokens [] (dataTokens, remainingTokens) = Right (dataTokens, remainingTokens)
checkDataTokens (x : xs) (dataTokens, remainingTokens) = do
    (token, tokens) <- nextToken remainingTokens
    if isValid (tokenType token) x
        then
            if isValidator x || isJust (tokenData token)
                then checkDataTokens xs (dataTokens ++ [token], tokens)
                else checkDataTokens xs (dataTokens, tokens)
        else expectedError token x

class ASTNode a where
    parse :: TokenStream -> Either ParseError (a, TokenStream)
    generate :: ASTNode a => a -> String

unaryOp = TokenValidator [Negate, BitwiseComplement, LogicalNegate]

data Expr = Integer Int | UnaryOp TokenEnum Expr deriving (Show)

instance ASTNode Expr where
    parse tokens = do
        (token, tokens) <- nextToken tokens
        if tokenType token == IntLiteral
            then return (Integer $ read $ fromJust $ tokenData token, tokens)
            else
                if tokenType token `isValid` unaryOp
                    then do
                        (expression, tokens) <- parse tokens :: Either ParseError (Expr, TokenStream)
                        return (UnaryOp (tokenType token) expression, tokens)
                    else Left (ParseError (Just token) "Expected Expression")

newtype Statement = Statement {expr :: Expr} deriving (Show)

instance ASTNode Statement where
    parse tokens = do
        tokens <- checkTokens [ReturnKeyword] tokens
        (expr, tokens) <- parse tokens
        tokens <- checkTokens [Semicolon] tokens
        return (Statement expr, tokens)

data FnDec = FnDec
    { name :: String
    , body :: Statement
    }
    deriving (Show)

instance ASTNode FnDec where
    parse tokens = do
        (dataTokens, tokens) <- checkDataTokens [IntKeyword, Ident, OpenParen, CloseParen, OpenBrace] ([], tokens)
        (body, tokens) <- parse tokens
        tokens <- checkTokens [CloseBrace] tokens
        return (FnDec (fromJust $ tokenData $ head dataTokens) body, tokens)

newtype Program = Program {fn :: FnDec} deriving (Show)

instance ASTNode Program where
    parse tokens = do
        (fnDec, tokens) <- parse tokens
        return (Program fnDec, tokens)

parseProgram :: TokenStream -> Either ParseError Program
parseProgram tokens = do
    (program, _) <- parse tokens
    return program