{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# HLINT ignore "Redundant if" #-}

module SemanticAnalyzer.DependencyList.Internal where

import Data.Array
import Util
import Debug.Trace

data DependencyListItem a = DepListSingle a | DepListCycle [a]
    deriving (Eq, Show)

data DependencyMatrix a = DependencyMatrix 
    { dmMatrixItems :: [DependencyListItem a]
    , dmMatrix :: Array (Int, Int) Bool
    } deriving (Eq)

instance (Show a) => Show (DependencyMatrix a) where
    show (DependencyMatrix items matrix)
        = let n = snd $ snd $ bounds matrix in
        show items 
        ++ "\n" 
        ++ unlines [ [if matrix ! (i, j) then '1' else '0'
                     | i <- [1..n]
                     ]
                   | j <- [1..n]
                   ]

mkDependencyMatrix :: (Eq a) => [a] -> (a -> a -> Bool) -> DependencyMatrix a
mkDependencyMatrix items dependsOn
    = mergeCycles $ DependencyMatrix (map DepListSingle items) allConnections
  where
    n = length items
    initial =
        array
            ((1, 1), (n, n))
            [ ((i, j), i == j 
              || ((items !! (i - 1)) `dependsOn` (items !! (j - 1))))
            | i <- [1 .. n]
            , j <- [1 .. n]
            ]
    allConnections = iterate pow2 initial !! stepsNeeded
    pow2 ini =
        array
            ((1, 1), (n, n))
            [ ((i, j), ini ! (i, j) 
              || or [ini ! (k, j) && ini ! (i, k)
                    | k <- [1 .. n]
                    ])
            | i <- [1 .. n]
            , j <- [1 .. n]
            ]
    stepsNeeded =
        fst $ head $
            dropWhile ((< n) . snd) $
                map (\m -> (m, (m ^ (2 :: Int) + m) `div` 2)) [1 ..]

mergeCycles :: (Eq a) => DependencyMatrix a -> DependencyMatrix a
mergeCycles depMat = mergeCyclesIter depMat startingN 1 2
  where
    startingN = snd $ snd $ bounds $ dmMatrix depMat
    -- it's assumed that x < y
    mergeRowCols :: Int -> Int -> Array (Int, Int) Bool -> Array (Int, Int) Bool
    mergeRowCols x y mat =
        let n = snd $ snd $ bounds mat
        in  array ((1, 1), (n - 1, n - 1)) 
            [ ((i, j), 
                if i == j                 then True                         else
                if i == (y - 1) && j >= x then 
                    mat ! (x, j + 1) || mat ! (y, j + 1)                    else
                if i == (y - 1)           then mat ! (x, j) || mat ! (y, j) else
                if j == (y - 1) && i >= x then 
                    mat ! (i + 1, x) || mat ! (i + 1, y)                    else
                if j == (y - 1)           then mat ! (i, x) || mat ! (i, y) else
                if i >= x && j >= x       then mat ! (i + 1, j + 1)         else
                if i >= x                 then mat ! (i + 1, j)             else
                if j >= x                 then mat ! (i, j + 1)             else
                                               mat ! (i, j)
              )
            | i <- [1..n-1]
            , j <- [1..n-1]
            ]
    mergeItems 
        :: (Eq a) => Int -> Int -> [DependencyListItem a] 
        -> [DependencyListItem a]
    mergeItems 1 y (a:as) = mergeItemWith a (y - 1) as
    mergeItems x y (a:as) = a : mergeItems (x - 1) (y - 1) as
    mergeItems _ 1 _      = error "index error in mergeItems"
    mergeItems _ _ []     = error "element not found error in mergeItems"
    mergeItemWith
        :: (Eq a) => DependencyListItem a -> Int 
        -> [DependencyListItem a] -> [DependencyListItem a]
    mergeItemWith a 1 (b:bs) = (a `itMerge` b) : bs
    mergeItemWith a y (b:bs) = b : mergeItemWith a (y-1) bs
    mergeItemWith _ _ []     = []
    mergeCyclesIter 
        :: (Eq a) => DependencyMatrix a -> Int -> Int -> Int 
        -> DependencyMatrix a
    mergeCyclesIter depMat'@(DependencyMatrix items matrix) n x y
        | x >= n = depMat'
        -- cycle detected
        | matrix ! (x, y) && matrix ! (y, x)
            = mergeCyclesIter 
            ( DependencyMatrix 
                (mergeItems x y items) (mergeRowCols x y matrix)
            ) (n - 1) x (x + 1)
        | y >= n    = mergeCyclesIter depMat' n (x + 1) (x + 2)
        | otherwise = mergeCyclesIter depMat' n x (y + 1)

createOrdering :: DependencyMatrix a -> DependencyList a
createOrdering depMat 
    = DependencyList 
    $ createOrderingIter initialN 1 depMat
  where
    initialN = snd $ snd $ bounds $ dmMatrix depMat
    deleteRowCols :: Int -> Array (Int, Int) a -> Array (Int, Int) a
    deleteRowCols x mat = let n = snd $ snd $ bounds mat
        in array ((1, 1),(n - 1, n - 1))
           [ ((i, j), 
             if i >= x && j >= x then mat ! (i + 1, j + 1) else
             if i >= x           then mat ! (i + 1, j)     else
             if j >= x           then mat ! (i, j + 1)     else
                                      mat ! (i, j))
           | i <- [1..n-1]
           , j <- [1..n-1]
           ]
    deleteItem :: Int -> [a] -> [a]
    deleteItem 1 (_:as) = as
    deleteItem x (a:as) = a : deleteItem (x - 1) as
    deleteItem _ []     = error "deleteItem: Index out of bounds"
    isLeaf :: Int -> Int -> Array (Int, Int) Bool -> Bool
    isLeaf n x mat 
        = sum [if mat ! (x, j) then 1 else 0 | j <- [1..n]] == (1 :: Int)
    createOrderingIter 
        :: Int -> Int -> DependencyMatrix a -> [DependencyListItem a]
    createOrderingIter n x depMat'@(DependencyMatrix items matrix)
        | n == 0 = []
        | x > n = createOrderingIter n 1 depMat'
        | isLeaf n x matrix 
            = (items !! (x - 1)) 
            : createOrderingIter (n - 1) x 
                (DependencyMatrix (deleteItem x items) (deleteRowCols x matrix))
        | otherwise = createOrderingIter n (x + 1) depMat'


-- A list whose elements don't 'depend' on the ones coming after them
newtype DependencyList a = DependencyList [DependencyListItem a] 
    deriving (Show, Eq)

mkDependencyList :: (Eq a) => [a] -> (a -> [a]) -> DependencyList a
mkDependencyList vals depf =
    -- DependencyList $ foldr (helper [] . DepListSingle) [] vals
    createOrdering $ mkDependencyMatrix vals (matDependsFn depf)
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

matDependsFn :: (Eq a) => (a -> [a]) -> a -> a -> Bool
matDependsFn depf a b = b `elem` depf a

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
