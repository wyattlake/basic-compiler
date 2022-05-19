module Generator () where

import Parser (ASTNode, Expr (..), FnDec (body, name), Program (fn), Statement (expr))

-- generate :: Program -> String
-- generate program =
--     "\t.globl\t_" ++ fnName
--         ++ "\n\
--            \_"
--         ++ fnName
--         ++ ":\n\tmovl\t$"
--         ++ show returnValue
--         ++ ", %eax\n\tretq"
--   where
--     fnName = name $ fn program
--     returnValue = value $ expr $ body $ fn program