module AST.Node (ASTNode (..)) where

import Parsing
import Token

class ASTNode a where
    parse :: TokenStream -> Either ParseError (a, TokenStream)
    generate :: a -> String