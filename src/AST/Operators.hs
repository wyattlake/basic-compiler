module AST.Operators (UnaryOp (..), unaryOpValidator) where

import AST.Node
import Parsing
import Token

-- <unary_op> ::= "!" | "~" | "-"
newtype UnaryOp = UnaryOp {op :: TokenEnum} deriving (Show)
unaryOpValidator = TokenValidator [Negate, BitwiseComplement, LogicalNegate]

instance ASTNode UnaryOp where
    parse tokens = error "unreachable" -- Parsing occurs in Expression to prevent cyclical imports

    generate unaryOp = case op unaryOp of
        Negate -> "    neg     %eax\n"
        BitwiseComplement -> "    not     %eax\n"
        LogicalNegate -> "    cmpl    $0, %eax\n    movl    $0, %eax\n    sete    %al\n"
        _ -> error "unreachable"