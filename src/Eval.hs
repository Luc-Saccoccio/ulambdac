{-# LANGUAGE OverloadedStrings #-}
module Eval (eval) where

import Parser
import Text.Megaparsec

prettyShow :: Bool -> LambdaTree -> String
prettyShow explicit = if explicit then prettyE else pretty
  where
    prettyE :: LambdaTree -> String
    prettyE (Variable v) = [v]
    prettyE (Lambda x b@(App _ _)) = 'λ' : x : ".(" ++ prettyE b ++ ")"
    prettyE (Lambda x b) = 'λ' : x : '.' : prettyE b
    prettyE (App f@(Lambda _ _) (Variable v)) = '(' : prettyE f ++ [')', v]
    prettyE (App f@(Lambda _ _) a) = '(' : prettyE f ++ ") (" ++ prettyE a ++ ")"
    prettyE (App (Variable v) (Variable w)) = ['(', v, w, ')']
    prettyE (App (Variable v) a) = v : '(' : prettyE a ++ ")"
    prettyE (App f a) = '(' : prettyE f ++ prettyE a ++ ")"
    prettyE (Alias x m) = '(' : x : " := " ++ prettyE m ++ ")"
    pretty :: LambdaTree -> String
    pretty (Variable v) = [v]
    pretty (Lambda x b) = 'λ' : x : '.' : pretty b
    pretty (App f@(Lambda _ _) (Variable v)) = '(' : pretty f ++ [')', v]
    pretty (App f@(Lambda _ _) a) = '(' : pretty f ++ ") (" ++ pretty a ++ ")"
    pretty (App (Variable v) (Variable w)) = [v, w]
    pretty (App f a@(App _ _)) = pretty f ++ "(" ++ pretty a ++ ")"
    pretty (App f a) = pretty f ++ pretty a
    pretty (Alias x m) = x : " := " ++ pretty m

eval :: Bool -> String -> String
eval b s =
  case parseExpr "repl" s of
    Left e -> errorBundlePretty e
    Right l -> prettyShow b l
