module Main (main) where

import Eval

main :: IO ()
main = interact (eval False)
