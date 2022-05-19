module Lib (
    compile,
) where

import Control.Exception
import Core
import Data.Either (fromRight)
import Data.List
import Generator
import Lexer
import Parser
import System.Environment
import Token

compile :: IO ()
compile = do
    args <- getArgs
    path <- try $ evaluate $ head args :: IO (Either SomeException String)
    case path of
        Left _ -> putStrLn "Error: Must specify target file"
        Right path -> do
            fileString <-
                try $ readFile path :: IO (Either SomeException String)
            case fileString of
                Left error -> putStrLn $ "Error: " ++ show error
                Right file -> do
                    putStrLn $ "Compiling " ++ path ++ "..."
                    case Lexer.lex file "" startPos of
                        Left error -> putStrLn $ "Lex Error: " ++ show error
                        Right tokens -> do
                            print tokens
                            case parseProgram tokens of
                                Left error -> putStrLn $ "Parse Error: " ++ show error
                                Right program -> do
                                    print program

--         writeFile (take (length path - 1) path ++ "s") (generate program)