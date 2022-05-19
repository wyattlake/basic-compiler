module AST.Expression (Expr (..)) where

import AST.Node
import AST.Operators
import Data.Maybe
import Parsing
import Token

-- <exp> ::= <unary_op> <exp> | <int>
data Expr = UnaryOp TokenEnum Expr | Int Int deriving (Show)

instance ASTNode Expr where
    parse tokens = do
        (token, tokens) <- nextToken tokens
        if tokenType token == IntLiteral
            then return (Int $ read $ fromJust $ tokenData token, tokens)
            else
                if tokenType token `isValid` unaryOp
                    then do
                        (expression, tokens) <- parse tokens :: Either ParseError (Expr, TokenStream)
                        return (UnaryOp (tokenType token) expression, tokens)
                    else Left (ParseError (Just token) "Expected Expression")
