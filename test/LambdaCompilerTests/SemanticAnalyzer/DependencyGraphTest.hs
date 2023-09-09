{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module LambdaCompilerTests.SemanticAnalyzer.DependencyGraphTest (spec) where

import SemanticAnalyzer.DependencyGraph.Internal
import Test.Hspec
import Test.QuickCheck
import Util

spec :: Spec
spec = do
  describe "DependencyGraph" $ do
    it "does not contain duplicate definitions" $
      property (testDepCycleCantHaveDuplicates :: DependencyGraph Int -> Bool)
    it "does not contain duplicate dependencies" $
      property (testDepGraphDepsCantHaveDuplicates :: DependencyGraph Int -> Bool)
    it "does not contain duplicate nodes in different cycles" $
      property
        ( \(DepGraphList xs) -> case xs of
            [] -> True
            [_] -> True
            (x : y : _) -> testDepGraphNodeCantAppearInDifferentCycles x (y :: DependencyGraph Int)
        )
    it "does not contain nodes in cycles that also appear in trees" $
      property
        ( \(DepGraphList xs) -> case xs of
            [] -> True
            [_] -> True
            (x : y : _) -> testDepGraphNodeCantAppearInDifferentCycles x (y :: DependencyGraph Int)
        )
  describe "+-+" $ do
    it "has no duplicates" $ do
      property ((\(NoDups x) (NoDups y) -> hasNoDups $ x +-+ y) :: NoDups Int -> NoDups Int -> Bool)

newtype NoDups a = NoDups [a] deriving (Show)

newtype DepGraphList a = DepGraphList [DependencyGraph a] deriving (Show)

instance (Eq a, Arbitrary a) => Arbitrary (NoDups a) where
  arbitrary = do
    NoDups . removeDups <$> arbitrary

instance Arbitrary (DependencyGraph Int) where
  arbitrary = do
    relsize <- (+ 1) . (`mod` 100) . abs <$> arbitrary
    relsizesq <- (+ 1) . (`mod` (relsize * relsize)) . abs <$> arbitrary
    rel <-
      filter (uncurry (/=))
        <$> vectorOf
          relsizesq
          ( do
              a <- (+ 1) . (`mod` relsize) . abs <$> arbitrary
              b <- (+ 1) . (`mod` relsize) . abs <$> arbitrary
              return (a, b)
          )
    case rel of
      [] -> do
        let depf = const []
        return $ fst $ mkDependencyGraphH depf [] 1
      ((a, _) : _) -> do
        let depf = makeDepf rel
        return $ fst $ mkDependencyGraphH depf [] a

instance Arbitrary (DepGraphList Int) where
  arbitrary = do
    relsize <- (+ 2) . (`mod` 99) . abs <$> arbitrary
    relsizesq <- (+ 1) . (`mod` (relsize * relsize)) . abs <$> arbitrary
    rel <-
      filter (uncurry (/=))
        <$> vectorOf
          relsizesq
          ( do
              a <- (+ 1) . (`mod` relsize) . abs <$> arbitrary
              b <- (+ 1) . (`mod` relsize) . abs <$> arbitrary
              return (a, b)
          )
    let elems = [1 .. relsize]
    let depf = makeDepf rel
    return $ DepGraphList $ getDepGraphs elems depf

-- Unit tests

makeDepf :: [(Int, Int)] -> (Int -> [Int])
makeDepf rel a = removeDups $ map snd $ filter ((== a) . fst) (filter ((/= a) . snd) rel)

hasNoDups :: (Eq a) => [a] -> Bool
hasNoDups (a : as)
  | a `elem` as = False
  | otherwise = hasNoDups as
hasNoDups [] = True

removeDups :: (Eq a) => [a] -> [a]
removeDups [] = []
removeDups (a : as)
  | a `elem` as = removeDups as
  | otherwise = a : removeDups as

testDepCycleCantHaveDuplicates :: (Eq a) => DependencyGraph a -> Bool
testDepCycleCantHaveDuplicates (DependencyTree _ _) = True
testDepCycleCantHaveDuplicates (DependencyCycle nodes _) = hasNoDups nodes

testDepGraphDepsCantHaveDuplicates :: (Eq a) => DependencyGraph a -> Bool
testDepGraphDepsCantHaveDuplicates (DependencyTree _ deps) = hasNoDups deps
testDepGraphDepsCantHaveDuplicates (DependencyCycle _ deps) = hasNoDups deps

testDepGraphNodeCantAppearInDifferentCycles :: (Eq a) => DependencyGraph a -> DependencyGraph a -> Bool
testDepGraphNodeCantAppearInDifferentCycles (DependencyCycle anodes _) (DependencyCycle bnodes _) =
  all (`notElem` bnodes) anodes && all (`notElem` anodes) bnodes
    || all (`elem` bnodes) anodes && all (`elem` anodes) bnodes
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
field3Elems = [1 .. 6]

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
field4Elems = [1 .. 7]

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
field5Elems = [1 .. 7]

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
field6Elems = [1 .. 8]

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
testDepGraphs graphs =
  all testDepGraphDepsCantHaveDuplicates graphs
    && all testDepCycleCantHaveDuplicates graphs
    && all (uncurry testDepGraphNodeCantAppearInDifferentCycles) [(a, b) | a <- graphs, b <- graphs]
    && all (uncurry testNodeInACycleCantAppearInATree) [(a, b) | a <- graphs, b <- graphs]

testField :: (Eq a) => [a] -> (a -> a -> Bool) -> Bool
testField elems rel = testDepGraphs (getDepGraphs elems (mkDepf elems rel))

semanticAnalyzerDependencyGraphTests :: [Bool]
semanticAnalyzerDependencyGraphTests =
  [ testField field1Elems field1,
    testField field2Elems field2,
    testField field3Elems field3,
    testField field4Elems field4,
    testField field5Elems field5,
    testField field6Elems field6
  ]
