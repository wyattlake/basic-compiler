module Core (
    Pos (..),
    nextChar,
    nextLine,
    nextPos,
    startPos,
) where

data Pos = Pos
    { line :: Int
    , column :: Int
    }
    deriving (Eq, Show)

startPos = Pos 1 1

nextPos :: Pos -> Char -> Pos
nextPos pos '\n' = nextLine pos
nextPos pos _ = nextChar pos

nextChar :: Pos -> Pos
nextChar (Pos l c) = Pos l (c + 1)

nextLine :: Pos -> Pos
nextLine (Pos l c) = Pos (l + 1) 1
