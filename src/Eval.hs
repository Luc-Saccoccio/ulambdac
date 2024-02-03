{-# LANGUAGE TupleSections #-}
module Eval (subterms, showSubterms, freeVariables, showFreeVariables, autoreduc) where

import Def
import qualified Data.HashSet as HS
import Data.HashMap.Strict
import Data.Maybe (isJust, fromMaybe)

type LambdHashTree = HashMap Identifier LambdaTree
type IdentifierSet = HS.HashSet Identifier
type LambdHashSet = HS.HashSet LambdaTree

subterms :: LambdaTree -> LambdHashSet
subterms t =
  case t of
    Variable _ -> HS.singleton t
    Lambda _ f -> HS.insert t (subterms f)
    App f g -> HS.insert t (subterms f `HS.union` subterms g)
    Alias _ f -> HS.insert t (subterms f)

freeVariables :: LambdaTree -> IdentifierSet
freeVariables t =
  case t of
    Variable x -> HS.singleton x
    Lambda x f -> HS.delete x (freeVariables f)
    App f g -> freeVariables f `HS.union` freeVariables g
    Alias x f -> HS.insert x $ freeVariables f

showFreeVariables :: IdentifierSet -> String
showFreeVariables = unwords . fmap (:[]) . HS.toList

showSubterms :: LambdHashSet -> String
showSubterms = unlines . fmap (('\t':) . prettyShow False) . HS.toList

whileMaybe :: (a -> Maybe a) -> a -> a
whileMaybe f x =
  case f x of
    Nothing -> x
    Just x' -> whileMaybe f x'

autoreduc :: LambdaTree -> LambdaTree
autoreduc = whileMaybe (autoreduc' empty)

autoreduc' :: LambdHashTree -> LambdaTree -> Maybe LambdaTree
autoreduc' lht (Variable c) = lht !? c
autoreduc' _ (Alias _ _) = Just (Lambda '?' $ Variable '?') -- TODO
autoreduc' lht (App f g) =
  case f of
    Variable c -> (autoreduc' lht . (`App` g)) =<< (lht !? c)
    Alias c t -> pure $ whileMaybe (autoreduc' (insert c t lht)) g
    Lambda x fx -> autoreduc' (insert x g lht) fx
    app ->
      let f' = autoreduc' lht app
          g' = autoreduc' lht g
       in if any isJust [f', g']
             then autoreduc' lht (App (fromMaybe f f') (fromMaybe g g'))
             else Nothing
autoreduc' lht (Lambda x f) = Lambda x <$> autoreduc' lht f
