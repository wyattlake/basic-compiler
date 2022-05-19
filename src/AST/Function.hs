module AST.Function (FnDec (..)) where

import AST.Node
import AST.Statement
import Data.Maybe
import Parsing
import Token

-- <function> ::= "int" <id> "(" ")" "{" <statement> "}"
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

    generate fn = "    .globl _" ++ fnName ++ "\n_" ++ fnName ++ ":\n" ++ generate (body fn) where fnName = name fn