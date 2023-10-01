module LambdaCompilerTests.SemanticAnalyzer.DependencyListTests(spec) where

import Test.Hspec
import Test.QuickCheck
import Util
import SemanticAnalyzer.DependencyList.Internal

spec :: Spec
spec = do
  describe "DependencyList" $ do
    it "contains elements exactly once" $
      property (\(ArbList (DependencyList as)) -> hasNoDups $ arrayify as)

newtype ArbList = ArbList (DependencyList Int) deriving (Show)

instance Arbitrary ArbList where
  arbitrary = do
    relsize <- (+10) . (`mod` 100) . abs <$> arbitrary
    relsizesq <- (`mod` (relsize * relsize)) . abs <$> arbitrary
    let conform = (+1) . (`mod` (relsize - 1))
    rel <- take relsizesq . map (fstMap conform . sndMap conform) <$> arbitrary
    let depf a' = foldr (\(a, b) acc -> if a == a' then b : acc else if b == a' then a : acc else acc) [] rel
    return $ ArbList $ mkDependencyList [1..relsize] depf

arrayify :: [DependencyListItem a] -> [a]
arrayify [] = []
arrayify ((DepListSingle a):as) = a : arrayify as
arrayify ((DepListCycle as):bs) = as ++ arrayify bs

hasNoDups :: (Eq a) => [a] -> Bool
hasNoDups [] = True
hasNoDups (a:as)
  | a `elem` as = False
  | otherwise = hasNoDups as
