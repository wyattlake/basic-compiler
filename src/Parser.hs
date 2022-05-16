module Parser
    () where

data Expr = Int

newtype Statement = Return Expr

data FnDec = FnDec
    { name :: String
    , body :: Statement
    }

newtype Program = Program FnDec
