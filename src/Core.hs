module Core
    ( Pos(..)
    ) where

data Pos = Pos
    { line   :: Int
    , column :: Int
    }
    deriving (Eq, Show)
