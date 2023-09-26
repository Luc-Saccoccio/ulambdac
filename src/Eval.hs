module Eval (subterms, showSubterms, freeVariables, showFreeVariables) where

import Def
import Data.HashSet

subterms :: LambdaTree -> HashSet LambdaTree
subterms t =
  case t of
    Variable _ -> singleton t
    Lambda _ f -> insert t (subterms f)
    App f g -> insert t (subterms f `union` subterms g)
    Alias _ f -> insert t (subterms f)

freeVariables :: LambdaTree -> HashSet Identifier
freeVariables t =
  case t of
    Variable x -> singleton x
    Lambda x f -> delete x (freeVariables f)
    App f g -> freeVariables f `union` freeVariables g
    Alias x f -> insert x $ freeVariables f

showFreeVariables :: HashSet Identifier -> String
showFreeVariables = unwords . fmap (:[]) . toList

showSubterms :: HashSet LambdaTree -> String
showSubterms = unlines . fmap (('\t':) . prettyShow False) . toList
