{-# LANGUAGE TupleSections #-}
module Eval (subterms, showSubterms, freeVariables, showFreeVariables, autoreduc, showBindings) where

import Def
import qualified Data.HashSet as HS
import Data.HashMap.Strict
import Data.Maybe (isJust, fromMaybe)

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
showSubterms = unlines . fmap (('\t':) . prettyShow) . HS.toList

showBindings :: LambdHashTree -> String
showBindings = unlines . foldrWithKey' f []
  where
    f :: Identifier -> LambdaTree -> [String] -> [String]
    f k v = (('\t':k:' ':':':'=':' ':prettyShow v):) -- "k := v"

whileMaybe :: (a -> Maybe a) -> a -> a
whileMaybe f x = maybe x (whileMaybe f) (f x)

-- For debugs purposes :
-- whileMaybe . (traceWith (maybe "Nothing" prettyShow) .) . autoreduc'
autoreduc :: LambdHashTree -> LambdaTree -> LambdaTree
autoreduc = whileMaybe . autoreduc'

autoreduc' :: LambdHashTree -> LambdaTree -> Maybe LambdaTree
autoreduc' lht (Variable c) = lht !? c
autoreduc' _ (Alias _ _) = Just (Lambda '?' $ Variable '?')
autoreduc' lht (App f g) =
  case f of
    Alias c t -> pure $ whileMaybe (autoreduc' (insert c t lht)) g
    Lambda x fx -> pure . fromMaybe fx $ autoreduc' (insert x g lht) fx
    app ->
      let f' = autoreduc' lht app
          g' = autoreduc' lht g
       in if any isJust [f', g']
             then Just (App (fromMaybe f f') (fromMaybe g g'))
             else Nothing
autoreduc' lht (Lambda x f) = Lambda x <$> autoreduc' (delete x lht) f
