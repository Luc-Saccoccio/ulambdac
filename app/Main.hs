module Main (main) where

import Control.Monad
import Def
import Eval
import Parser
import System.Console.Isocline
import System.Exit
import Text.Megaparsec

eval :: Bool -> String -> IO ()
eval b s = case parseCommand "repl" s of
  Left  e -> putStrLn $ errorBundlePretty e
  Right r -> case r of
    Subterms  l -> putStr $ showSubterms (subterms l)
    Redexes   l -> putStrLn $ "Redexes " ++ prettyShow b l
    FV        l -> putStrLn $ '\t' : showFreeVariables (freeVariables l)
    AutoReduc l -> putStrLn $ "AutoReduc " ++ prettyShow b l
    ManReduc  l -> putStrLn $ "ManReduc " ++ prettyShow b l
    None      l -> putStrLn $ prettyShow b l
    Load      f -> putStrLn $ "Load " ++ show f
    Reload      -> putStrLn "Reload"
    Quit        -> exitSuccess
    Edit f      -> putStrLn $ "Edit " ++ show f

main :: IO ()
main = forever $ readline "λ" >>= eval False
