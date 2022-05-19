module AST.Program (Program (..), parseProgram) where

import AST.Function
import AST.Node
import Parsing
import Token

-- <program> ::= <function>
newtype Program = Program {fn :: FnDec} deriving (Show)

instance ASTNode Program where
    parse tokens = do
        (fnDec, tokens) <- parse tokens
        return (Program fnDec, tokens)

parseProgram :: TokenStream -> Either ParseError Program
parseProgram tokens = do
    (program, _) <- parse tokens
    return program