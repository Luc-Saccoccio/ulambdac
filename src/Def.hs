{-# LANGUAGE DeriveGeneric #-}
module Def
  ( LambdaTree(..)
  , LambdHashTree
  , IdentifierSet
  , LambdHashSet
  , Command(..)
  , Identifier
  , prettyShow
  )
where

import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import GHC.Generics (Generic)

type Identifier = Char

data LambdaTree
  = App LambdaTree LambdaTree
  | Lambda Identifier LambdaTree
  | Variable Identifier
  | Alias Identifier LambdaTree
  deriving (Show, Eq, Generic)

instance Hashable LambdaTree

data Command
  = Subterms LambdaTree
  | Redexes LambdaTree
  | FV LambdaTree
  | AutoReduc LambdaTree
  | ManReduc LambdaTree
  | None LambdaTree
  | Bindings
  | Quit
  | Help
  | Delete Char

type LambdHashTree = HashMap Identifier LambdaTree
type IdentifierSet = HashSet Identifier
type LambdHashSet = HashSet LambdaTree

prettyShow :: LambdaTree -> String
prettyShow (Variable v) = [v]
prettyShow (Lambda x b@(App _ _)) = 'λ' : x : '.' : '(' : prettyShow b ++ ")"
prettyShow (Lambda x b) = 'λ' : x : '.' : prettyShow b
prettyShow (App f@(Lambda _ _) (Variable v)) = '(' : prettyShow f ++ [')', v]
prettyShow (App f@(Lambda _ _) a) = '(' : prettyShow f ++ ") (" ++ prettyShow a ++ ")"
prettyShow (App (Variable v) (Variable w)) = ['(', v, w, ')']
prettyShow (App (Variable v) a) = v : '(' : prettyShow a ++ ")"
prettyShow (App f a) = '(' : prettyShow f ++ prettyShow a ++ ")"
prettyShow (Alias x m) = '(' : x : " := " ++ prettyShow m ++ ")"
