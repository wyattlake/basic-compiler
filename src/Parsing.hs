module Parsing (ParseError (..), nextToken, nextTokens, checkDataTokens, checkTokens, isValid) where

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