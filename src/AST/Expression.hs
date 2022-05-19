module AST.Expression (Expr (..)) where

import AST.Node
import AST.Operators
import Data.Maybe
import Parsing
import Token

-- <exp> ::= <unary_op> <exp> | <int>
data Expr = UnaryOperation UnaryOp Expr | Value Int deriving (Show)

instance ASTNode Expr where
    parse tokens = do
        (token, tokens) <- nextToken tokens
        if tokenType token == IntLiteral
            then return (Value $ read $ fromJust $ tokenData token, tokens)
            else
                if tokenType token `isValid` unaryOpValidator
                    then do
                        (expression, tokens) <- parse tokens :: Either ParseError (Expr, TokenStream)
                        return (UnaryOperation (UnaryOp $ tokenType token) expression, tokens)
                    else Left (ParseError (Just token) "Expected Expression")

    generate expr = case expr of
        Value value -> "    movl    $" ++ show value ++ ", %eax\n"
        UnaryOperation unaryOp expr -> generate expr ++ generate unaryOp