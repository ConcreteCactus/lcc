{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module SemanticAnalyzer.DependencyList.Internal where

import Util

data DependencyListItem a
  = DepListSingle a
  | DepListCycle [a]
  deriving (Eq, Show)

-- A list whose elements don't 'depend' on the ones coming after them
newtype DependencyList a = DependencyList [DependencyListItem a] deriving Show

mkDependencyList :: (Eq a) => [a] -> (a -> [a]) -> DependencyList a
mkDependencyList vals depf = DependencyList 
    $ foldr (helper' [] . DepListSingle) [] vals
  where
    helper a [] = [a]
    helper a (b : bs)
      | not (b `dependsOn` a) = b : helper a bs
      | a `dependsOn` b = a `itMerge` b : bs
      | otherwise = a : (b : bs)
    helper' aft a [] = a : reverse aft
    helper' aft a (b : bs)
      | not (b `dependsOn` a) = b : helper' aft a bs
      | a `dependsOn` b = helper' aft (a `itMerge` b) bs
      | otherwise = helper' (b : aft) a bs
    dependsOn = dependsFn depf

dependsFn ::
  (Eq a) =>
  (a -> [a]) ->
  DependencyListItem a ->
  DependencyListItem a ->
  Bool
dependsFn depf (DepListSingle a) (DepListSingle b) = b `elem` depf a
dependsFn depf (DepListSingle a) (DepListCycle bs) = any (`elem` depf a) bs
dependsFn depf (DepListCycle as) (DepListSingle b) = any ((b `elem`) . depf) as
dependsFn depf (DepListCycle as) (DepListCycle bs) =
  or
    [b `elem` depf a | a <- as, b <- bs]

itMerge ::
  (Eq a) =>
  DependencyListItem a ->
  DependencyListItem a ->
  DependencyListItem a
itMerge (DepListSingle a) (DepListSingle b) =
    if a /= b then DepListCycle [a, b] else DepListSingle a
itMerge (DepListCycle as) (DepListSingle b) =
    if b `notElem` as then DepListCycle (b : as) else DepListCycle as
itMerge (DepListSingle a) (DepListCycle bs) =
    if a `notElem` bs then DepListCycle (a : bs) else DepListCycle bs
itMerge (DepListCycle as) (DepListCycle bs) = DepListCycle (as +-+ bs)

