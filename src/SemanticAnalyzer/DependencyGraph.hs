module SemanticAnalyzer.DependencyGraph () where

data (Eq a) => DependencyGraph a
  = DependencyTree a [DependencyGraph a]
  | DependencyCycle [a] [DependencyGraph a]
  deriving (Show)

instance (Eq a) => Eq (DependencyGraph a) where
  DependencyTree a _ == DependencyTree b _ = a == b
  DependencyCycle a _ == DependencyCycle b _ = a == b
  _ == _ = False

type PrevPath a = [a]

-- mkDependencyGraph :: (Eq a) => [a] -> (a -> [a]) -> [DependencyGraph a]
-- mkDependencyGraph vals depf = _

mkDependencyGraphH :: (Eq a) => (a -> [a]) -> PrevPath a -> a -> (DependencyGraph a, Int)
mkDependencyGraphH depf prevPath node = if node `elem` prevPath 
    then let dcycle = takeWhile (/=node) prevPath in
      (DependencyCycle [] [], length dcycle)
    else 
      let (prGraph, n) = foldr (procNexts node) (DependencyTree node [], 0) nexts in
        case prGraph of
          cy@(DependencyCycle a adeps) -> (cy, n-1)
          a -> (a, 0)
        
  where
    nexts = map (mkDependencyGraphH depf (node : prevPath)) (depf node)
    procNexts :: Eq a => a -> (DependencyGraph a, Int) -> (DependencyGraph a, Int) -> (DependencyGraph a, Int)
    procNexts _ (dta@(DependencyTree _ _), _) (DependencyTree node' ndeps, _) = (DependencyTree node' (dta : ndeps), 0)
    procNexts _ (dta@(DependencyTree _ _), _) (DependencyCycle nodes' ndeps, n) = (DependencyCycle nodes' (dta : ndeps), n)
    procNexts _ (dca@(DependencyCycle _ _), 0) (DependencyTree node' ndeps, _) = (DependencyTree node' (dca : ndeps), 0)
    procNexts _ (DependencyCycle nodes' cdeps, n) (DependencyTree node' ndeps, _) = (DependencyCycle (node' : nodes') (ndeps +-+ cdeps), n)
    procNexts _ (dca@(DependencyCycle _ _), 0) (DependencyCycle nodes' ndeps, n) = (DependencyCycle nodes' (dca : ndeps), n)
    procNexts currNode (DependencyCycle anodes' adeps, an) (DependencyCycle bnodes' bdeps, bn) =
      (DependencyCycle (currNode : (anodes' ++ bnodes')) (adeps +-+ bdeps), max an bn)

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

mkDepf :: [a] -> (a -> a -> Bool) -> (a -> [a])
mkDepf elems rel a = filter (rel a) elems

getDepGraphs :: (Eq a) => [a] -> (a -> [a]) -> [DependencyGraph a]
getDepGraphs elems deps = fst . mkDependencyGraphH deps [] <$> elems

testDepGraphs :: (Eq a) => [DependencyGraph a] -> Bool
testDepGraphs graphs = all testDepGraphDepsCantHaveDuplicates graphs && all testDepCycleCantHaveDuplicates graphs
  && all (uncurry testDepGraphNodeCantAppearInDifferentCycles) [(a, b) | a <- graphs, b <- graphs]
  && all (uncurry testNodeInACycleCantAppearInATree) [(a, b) | a <- graphs, b <- graphs]

testField :: (Eq a) => [a] -> (a -> a -> Bool) -> Bool
testField elems rel = testDepGraphs (getDepGraphs elems (mkDepf elems rel))

tests :: [Bool]
tests = [
    testField field1Elems field1,
    testField field2Elems field2,
    testField field3Elems field3
  ]
