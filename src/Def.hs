{-# LANGUAGE DeriveGeneric #-}
module Def
  ( LambdaTree(..)
  , Command(..)
  , Identifier
  , prettyShow
  )
where

import Data.Hashable
import GHC.Generics (Generic)

type Identifier = Char

data LambdaTree
  = App LambdaTree LambdaTree
  | Lambda Identifier LambdaTree
  | Variable Identifier
  | Alias Identifier LambdaTree
  deriving (Eq, Generic)

instance Hashable LambdaTree

data Command
  = Subterms LambdaTree
  | Redexes LambdaTree
  | FV LambdaTree
  | AutoReduc LambdaTree
  | ManReduc LambdaTree
  | None LambdaTree
  | Reload
  | Quit
  | Load [FilePath]
  | Edit FilePath

prettyShow :: Bool -> LambdaTree -> String
prettyShow explicit = if explicit then prettyE else pretty
  where
    prettyE :: LambdaTree -> String
    prettyE (Variable v) = [v]
    prettyE (Lambda x b@(App _ _)) = 'λ' : x : '.' : '(' : prettyE b ++ ")"
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
