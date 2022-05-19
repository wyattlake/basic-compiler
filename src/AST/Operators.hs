module AST.Operators (unaryOp) where

import AST.Node
import Token

-- <unary_op> ::= "!" | "~" | "-"
unaryOp = TokenValidator [Negate, BitwiseComplement, LogicalNegate]