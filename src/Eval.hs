{-# LANGUAGE OverloadedStrings #-}
module Eval (subterms, showSubterms) where

import Def
import Data.HashSet

subterms :: LambdaTree -> HashSet LambdaTree
subterms t =
  case t of
    Variable _ -> singleton t
    Lambda _ f -> insert t (subterms f)
    App f g -> insert t (subterms f `union` subterms g)
    Alias _ f -> insert t (subterms f)

showSubterms :: HashSet LambdaTree -> String
showSubterms = unlines . fmap (('\t':) . prettyShow False) . toList
