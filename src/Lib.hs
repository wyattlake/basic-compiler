module Lib
    ( main
    ) where

import           Control.Exception
import           Core
import           Data.List
import           Lexer
import           System.Environment
import           Token
import              Parser

main :: IO ()
main = do
    args <- getArgs
    path <- try $ evaluate $ head args :: IO (Either SomeException String)
    case path of
        Left  _    -> putStrLn "Error: Must specify target file"
        Right path -> do
            fileString <-
                try $ readFile path :: IO (Either SomeException String)
            case fileString of
                Left  error -> putStrLn $ "Error: " ++ show error
                Right file  -> do
                    putStrLn $ "Compiling " ++ path ++ "..."
                    case Lexer.lex file "" startPos of
                        Left  error  -> putStrLn $ "Lex Error: " ++ show error
                        Right tokens -> do
                            print tokens
