module AST.Statement (Statement (..)) where

import AST.Expression
import AST.Node
import Parsing
import Token

-- <statement> ::= "return" <exp> ";"
newtype Statement = Statement {expr :: Expr} deriving (Show)

instance ASTNode Statement where
    parse tokens = do
        tokens <- checkTokens [ReturnKeyword] tokens
        (expr, tokens) <- parse tokens
        tokens <- checkTokens [Semicolon] tokens
        return (Statement expr, tokens)
