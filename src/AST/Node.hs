{-# LANGUAGE ConstrainedClassMethods #-}

module AST.Node (ASTNode (..)) where

import Parsing
import Token

class ASTNode a where
    parse :: TokenStream -> Either ParseError (a, TokenStream)
    generate :: ASTNode a => a -> String