{-# LANGUAGE LambdaCase #-}
module SemanticAnalyzer.DependencyGraph (
      DependencyGraph,
      mkDependencyGraph,
      semanticAnalyzerDependencyGraphTests
    ) where

import Util

data (Eq a) => DependencyGraph a
  = DependencyTree a [DependencyGraph a]
  | DependencyCycle [a] [DependencyGraph a]

newtype (Eq a) => MkDepGEnv a = MkDepGEnv [a]

instance (Eq a) => Eq (DependencyGraph a) where
  DependencyTree a _ == DependencyTree b _ = a == b
  DependencyCycle a _ == DependencyCycle b _ = a == b
  _ == _ = False

instance (Eq a, Show a) => Show (DependencyGraph a) where
  show graph = showH graph 0

showH :: (Eq a, Show a) => DependencyGraph a -> Int -> String
showH (DependencyTree a deps) n = replicate (2*n) ' ' ++ show a ++ "\n" ++ concatMap (`showH` (n+1)) deps
showH (DependencyCycle nodes deps) n = replicate (2*n) ' ' ++ show nodes ++ "\n" ++ concatMap (`showH` (n+1)) deps

type PrevPath a = [a]

mkDependencyGraph :: (Eq a) => [a] -> (a -> [a]) -> [DependencyGraph a]
mkDependencyGraph vals depf = execState (mkDependencyGraphS vals depf) (MkDepGEnv [])

dgeSetWalked :: (Eq a) => a -> State (MkDepGEnv a) ()
dgeSetWalked a = do
  (MkDepGEnv env) <- get
  put $ MkDepGEnv $ a : env

dgeIsWalked :: (Eq a) => a -> State (MkDepGEnv a) Bool
dgeIsWalked a = do
  (MkDepGEnv env) <- get
  return $ a `elem` env

dependsOn :: (Eq a) => DependencyGraph a -> a -> Bool
dependsOn (DependencyTree a adeps) b = a == b || any (`dependsOn`b) adeps
dependsOn (DependencyCycle as asdeps) b = b `elem` as || any (`dependsOn`b) asdeps

mkDependencyGraphS :: (Eq a) => [a] -> (a -> [a]) -> State (MkDepGEnv a) [DependencyGraph a]
mkDependencyGraphS [] _ = return []
mkDependencyGraphS (a:as) depf = do
  walked <- dgeIsWalked a
  if walked then do
    mkDependencyGraphS as depf
  else do
    dgeSetWalked a
    next <- mkDependencyGraphS as depf
    let (ad, _) = mkDependencyGraphH depf [] a
    return $ ad : filter (\case
        DependencyCycle as' _ -> not $ any (ad`dependsOn`) as'
        DependencyTree a' _ -> not $ ad `dependsOn` a'
      ) next

mkDependencyGraphH :: (Eq a) => (a -> [a]) -> PrevPath a -> a -> (DependencyGraph a, Int)
mkDependencyGraphH depf prevPath node = if node `elem` prevPath 
    then let dcycle = takeWhile (/=node) prevPath in
      (DependencyCycle [] [], length dcycle + 1)
    else 
      let (prGraph, n) = foldr procNexts (DependencyTree node [], 0) nexts in
        case prGraph of
          cy@(DependencyCycle a adeps) -> (cy, n-1)
          a -> (a, 0)
        
  where
    nexts = map (mkDependencyGraphH depf (node : prevPath)) (depf node)
    procNexts :: Eq a => (DependencyGraph a, Int) -> (DependencyGraph a, Int) -> (DependencyGraph a, Int)
    procNexts (dta@(DependencyTree _ _), _) (DependencyTree node' ndeps, _) = (DependencyTree node' (dta : ndeps), 0)
    procNexts (dta@(DependencyTree _ _), _) (DependencyCycle nodes' ndeps, n) = (DependencyCycle nodes' (dta : ndeps), n)
    procNexts (dca@(DependencyCycle _ _), 0) (DependencyTree node' ndeps, _) = (DependencyTree node' (dca : ndeps), 0)
    procNexts (DependencyCycle nodes' cdeps, n) (DependencyTree node' ndeps, _) = (DependencyCycle (node' : nodes') (ndeps +-+ cdeps), n)
    procNexts (dca@(DependencyCycle _ _), 0) (DependencyCycle nodes' ndeps, n) = (DependencyCycle nodes' (dca : ndeps), n)
    procNexts (DependencyCycle anodes' adeps, an) (DependencyCycle bnodes' bdeps, bn) =
      (DependencyCycle (anodes' +-+ bnodes') (adeps +-+ bdeps), max an bn)

(+-+) :: (Eq a) => [a] -> [a] -> [a]
(a : as) +-+ bs
  | a `elem` bs = as +-+ bs
  | otherwise = a : (as +-+ bs)
[] +-+ bs = bs

-- Unit tests
hasNoDups :: (Eq a) => [a] -> Bool
hasNoDups (a : as)
  | a `elem` as = False
  | otherwise = hasNoDups as
hasNoDups [] = True

testDepCycleCantHaveDuplicates :: (Eq a) => DependencyGraph a -> Bool
testDepCycleCantHaveDuplicates (DependencyTree _ _) = True
testDepCycleCantHaveDuplicates (DependencyCycle nodes _) = hasNoDups nodes

testDepGraphDepsCantHaveDuplicates :: (Eq a) => DependencyGraph a -> Bool
testDepGraphDepsCantHaveDuplicates (DependencyTree _ deps) = hasNoDups deps
testDepGraphDepsCantHaveDuplicates (DependencyCycle _ deps) = hasNoDups deps

testDepGraphNodeCantAppearInDifferentCycles :: (Eq a) => DependencyGraph a -> DependencyGraph a -> Bool
testDepGraphNodeCantAppearInDifferentCycles (DependencyCycle anodes _) (DependencyCycle bnodes _) =
  (all (`notElem`bnodes) anodes && all (`notElem`anodes) bnodes)
  || (all (`elem`bnodes) anodes && all (`elem`anodes) bnodes)
testDepGraphNodeCantAppearInDifferentCycles _ _ = True

testNodeInACycleCantAppearInATree :: (Eq a) => DependencyGraph a -> DependencyGraph a -> Bool
testNodeInACycleCantAppearInATree (DependencyTree node _) (DependencyCycle nodes _) = node `notElem` nodes
testNodeInACycleCantAppearInATree (DependencyCycle nodes _) (DependencyTree node _) = node `notElem` nodes
testNodeInACycleCantAppearInATree _ _ = True

field1 :: Int -> Int -> Bool
field1 1 2 = True
field1 1 3 = True
field1 3 4 = True
field1 _ _ = False
field1Elems :: [Int]
field1Elems = [1, 2, 3, 4]

field2 :: Int -> Int -> Bool
field2 1 2 = True
field2 1 3 = True
field2 3 4 = True
field2 4 5 = True
field2 5 3 = True
field2 _ _ = False
field2Elems :: [Int]
field2Elems = [1, 2, 3, 4, 5]

-- 1 -> 2 -> 3 -> 4 -> 5 -> 3!

field3 :: Int -> Int -> Bool
field3 1 2 = True
field3 1 3 = True
field3 3 4 = True
field3 4 5 = True
field3 5 3 = True
field3 5 6 = True
field3 6 3 = True
field3 _ _ = False
field3Elems :: [Int]
field3Elems = [1..6]

-- 1 -> 2
--   -> 3 -> 4 -> 5 -> 3!
--                  -> 6 -> 3!

field4 :: Int -> Int -> Bool
field4 1 2 = True
field4 1 3 = True
field4 3 4 = True
field4 4 5 = True
field4 5 7 = True
field4 7 3 = True
field4 5 6 = True
field4 6 3 = True
field4 _ _ = False
field4Elems :: [Int]
field4Elems = [1..7]

-- 1 -> 2
--   -> 3 -> 4 -> 5 -> 7 -> 3!
--                  -> 6 -> 3!

field5 :: Int -> Int -> Bool
field5 1 2 = True
field5 1 3 = True
field5 3 4 = True
field5 4 5 = True
field5 5 7 = True
field5 7 2 = True
field5 5 6 = True
field5 6 3 = True
field5 _ _ = False
field5Elems :: [Int]
field5Elems = [1..7]

-- 1 -> 2
--   -> 3 -> 4 -> 5 -> 7 -> 2!
--                  -> 6 -> 3!

field6 :: Int -> Int -> Bool
field6 1 2 = True
field6 1 3 = True
field6 3 4 = True
field6 4 5 = True
field6 5 7 = True
field6 7 2 = True
field6 7 8 = True
field6 5 6 = True
field6 6 3 = True
field6 8 4 = True
field6 _ _ = False
field6Elems :: [Int]
field6Elems = [1..8]

-- 1 -> 2
--   -> 3 -> 4 -> 5 -> 6 -> 3!
--                  -> 7 -> 2!
--                       -> 8 -> 4!
mkDepf :: [a] -> (a -> a -> Bool) -> (a -> [a])
mkDepf elems rel a = filter (rel a) elems

getDepGraphs :: (Eq a) => [a] -> (a -> [a]) -> [DependencyGraph a]
getDepGraphs elems deps = fst . mkDependencyGraphH deps [] <$> elems

getFieldDepGraphs :: (Eq a) => [a] -> (a -> a -> Bool) -> [DependencyGraph a]
getFieldDepGraphs elems rel = getDepGraphs elems (mkDepf elems rel)

testDepGraphs :: (Eq a) => [DependencyGraph a] -> Bool
testDepGraphs graphs = all testDepGraphDepsCantHaveDuplicates graphs && all testDepCycleCantHaveDuplicates graphs
  && all (uncurry testDepGraphNodeCantAppearInDifferentCycles) [(a, b) | a <- graphs, b <- graphs]
  && all (uncurry testNodeInACycleCantAppearInATree) [(a, b) | a <- graphs, b <- graphs]

testField :: (Eq a) => [a] -> (a -> a -> Bool) -> Bool
testField elems rel = testDepGraphs (getDepGraphs elems (mkDepf elems rel))

semanticAnalyzerDependencyGraphTests :: [Bool]
semanticAnalyzerDependencyGraphTests = [
    testField field1Elems field1,
    testField field2Elems field2,
    testField field3Elems field3,
    testField field4Elems field4,
    testField field5Elems field5,
    testField field6Elems field6
  ]
