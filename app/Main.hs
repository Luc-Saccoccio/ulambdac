{-# LANGUAGE LambdaCase #-}
module Main (main) where

import Control.Monad
import Data.HashMap.Strict (empty, insert, delete)
import Data.IORef
import Def
import Eval
import Parser
import System.Console.Isocline
import System.Environment (getEnv, lookupEnv)
import System.Exit
import Text.Megaparsec (errorBundlePretty)

helpMsg :: String
helpMsg = "TODO"

historyLocation :: IO FilePath
historyLocation = maybe ((++ "/.cache/ulambdac_history") <$> getEnv "HOME") (pure . (++ "/ulambdac_history")) =<< lookupEnv "XDG_CACHE_HOME"

evalCommand :: IORef LambdHashTree -> Command -> IO ()
evalCommand lht (AutoReduc l) = do
  lht' <- readIORef lht
  putStrLn $ "AutoReduc " ++ prettyShow (autoreduc lht' l)
evalCommand lht Bindings = putStr . showBindings =<< readIORef lht
evalCommand lht (Delete c) = modifyIORef' lht (delete c)
evalCommand _ (FV l) = putStrLn $ '\t' : showFreeVariables (freeVariables l) -- FIXME
evalCommand _ Help = putStrLn helpMsg
evalCommand _ (ManReduc l) = putStrLn $ "ManReduc " ++ prettyShow l
evalCommand lht (None l) =
  case l of
    Alias m f -> modifyIORef' lht (insert m f)
    _ -> putStrLn $ prettyShow l
evalCommand _ Quit = exitSuccess
evalCommand _ (Redexes l) = putStrLn $ "Redexes " ++ prettyShow l
evalCommand _ (Subterms l) = putStr $ showSubterms (subterms l)

eval :: IORef LambdHashTree -> String -> IO ()
eval lht s = do
  case parseCommand "repl" s of
    Left  e -> putStrLn $ errorBundlePretty e
    Right r -> evalCommand lht r

main :: IO ()
main = do
  history <- historyLocation
  setHistory history 200
  lht <- newIORef empty
  forever $ readline "λ" >>= eval lht
