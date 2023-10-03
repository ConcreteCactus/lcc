module LambdaCompilerTests.SemanticAnalyzer.DependencyListTests (spec) where

import Data.List (intercalate)
import SemanticAnalyzer.DependencyList.Internal
import Test.Hspec
import Test.QuickCheck
import Util

spec :: Spec
spec = do
  describe "DependencyList" $ do
    it "contains elements exactly once" $
      property (\(ArbList (DependencyList as) _ _) -> hasNoDups $ arrayify as)
    it "contains relsize number of elements" $
      property
        ( \(ArbList (DependencyList as) size _) ->
            length (arrayify as) == size
        )
    it "'s elements don't depend on ones coming after them" $
      property
        ( \(ArbList (DependencyList as) _ depf) ->
            let dependsOn = dependsFn depf
             in fst $
                  foldr
                    ( \item (good, arr) ->
                        if good && not (any (item `dependsOn`) arr)
                          then (True, item : arr)
                          else (False, arr)
                    )
                    (True, [])
                    as
        )

data ArbList = ArbList (DependencyList Int) Int (Int -> [Int])

instance Show ArbList where
  show (ArbList dpList size depf) = show dpList ++ " > " ++ show size ++ "\n"
    ++ showdepf depf size

showdepf :: (Int -> [Int]) -> Int -> String
showdepf depf size =
  intercalate "\n" $
    map (\a -> show a ++ ": " ++ show (depf a)) [1 .. size]

instance Arbitrary ArbList where
  arbitrary = do
    relsize <- (+ 10) . (`mod` 100) . abs <$> arbitrary
    relsizesq <- (`mod` (relsize * relsize)) . (+ relsize) . abs <$> arbitrary
    let conform = (+ 1) . (`mod` (relsize - 1))
    rel <- take relsizesq . map (fstMap conform . sndMap conform) <$> arbitrary
    let depf a' = foldr (\(a, b) acc -> if a == a' then b : acc else if b == a' then a : acc else acc) [] rel
    return $ ArbList (mkDependencyList [1 .. relsize] depf) relsize depf

arrayify :: [DependencyListItem a] -> [a]
arrayify [] = []
arrayify ((DepListSingle a) : as) = a : arrayify as
arrayify ((DepListCycle as) : bs) = as ++ arrayify bs

hasNoDups :: (Eq a) => [a] -> Bool
hasNoDups [] = True
hasNoDups (a : as)
  | a `elem` as = False
  | otherwise = hasNoDups as
