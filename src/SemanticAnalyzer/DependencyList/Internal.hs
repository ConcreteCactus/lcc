{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module SemanticAnalyzer.DependencyList.Internal where

import Data.Array
import Debug.Trace
import Util

data DependencyListItem a = DepListSingle a | DepListCycle [a]
    deriving (Eq, Show)

type DependencyMatrix = Array (Int, Int) Bool

mkDependencyMatrix :: [a] -> (a -> a -> Bool) -> DependencyMatrix
mkDependencyMatrix items dependsOn = allConnections
  where
    n = length items
    initial =
        array
            ((1, 1), (n, n))
            [ ((i, j), i == j || ((items !! (i - 1)) `dependsOn` (items !! (j - 1))))
            | i <- [1 .. n]
            , j <- [1 .. n]
            ]
    allConnections = iterate pow2 initial !! stepsNeeded
    pow2 ini =
        array
            ((1, 1), (n, n))
            [ ( (i, j)
              , ini ! (i, j)
                    || or
                        [ ini ! (k, j) && ini ! (i, k)
                        | k <- [1 .. n]
                        ]
              )
            | i <- [1 .. n]
            , j <- [1 .. n]
            ]
    stepsNeeded =
        fst $ head $
            dropWhile ((< n) . snd) $
                map (\m -> (m, (m ^ (2 :: Int) + m) `div` 2)) [1 ..]

-- A list whose elements don't 'depend' on the ones coming after them
newtype DependencyList a = DependencyList [DependencyListItem a] deriving (Show)

mkDependencyList :: (Eq a, Show a) => [a] -> (a -> [a]) -> DependencyList a
mkDependencyList vals depf =
    DependencyList $ foldr (helper [] . DepListSingle) [] vals
  where
    helper aft a [] = a : aft
    helper aft a (b : bs)
        | not (b `dependsOn` a) = b : helper aft a bs
        | a `dependsOn` b = helper aft (a `itMerge` b) bs
        | otherwise = helper (b : aft) a bs
    dependsOn a b =
        -- trace (show a ++ " dependsOn? " ++ show b ++ " : " ++ show (dependsFn depf a b)) $
            dependsFn depf a b

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
    or [b `elem` depf a | a <- as, b <- bs]

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
