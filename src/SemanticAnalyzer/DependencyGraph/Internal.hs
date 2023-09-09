{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module SemanticAnalyzer.DependencyGraph.Internal where

import Util

data DependencyGraph a
  = DependencyTree a [DependencyGraph a]
  | DependencyCycle [a] [DependencyGraph a]

newtype MkDepGEnv a = MkDepGEnv [a]

instance (Eq a) => Eq (DependencyGraph a) where
  DependencyTree a _ == DependencyTree b _ = a == b
  DependencyCycle a _ == DependencyCycle b _ = a == b
  _ == _ = False

instance Functor DependencyGraph where
  fmap f (DependencyTree a adeps) = DependencyTree (f a) (fmap f `fmap` adeps)
  fmap f (DependencyCycle as asdeps) = DependencyCycle (fmap f as) (fmap f `fmap` asdeps)

instance Foldable DependencyGraph where
  foldr f b (DependencyTree a adeps) = f a (foldr (flip (foldr f)) b adeps)
  foldr f b (DependencyCycle as asdeps) = foldr f (foldr (flip (foldr f)) b asdeps) as

instance (Eq a, Show a) => Show (DependencyGraph a) where
  show graph = showH graph 0

showH :: (Eq a, Show a) => DependencyGraph a -> Int -> String
showH (DependencyTree a deps) n = replicate (2 * n) ' ' ++ show a ++ "\n" ++ concatMap (`showH` (n + 1)) deps
showH (DependencyCycle nodes deps) n = replicate (2 * n) ' ' ++ show nodes ++ "\n" ++ concatMap (`showH` (n + 1)) deps

type PrevPath a = [a]

mkDependencyGraph :: (Eq a) => [a] -> (a -> [a]) -> [DependencyGraph a]
mkDependencyGraph vals depf = execState (mkDependencyGraphS vals depf) (MkDepGEnv [])

dgeSetWalked :: a -> State (MkDepGEnv a) ()
dgeSetWalked a = do
  (MkDepGEnv env) <- get
  put $ MkDepGEnv $ a : env

dgeIsWalked :: (Eq a) => a -> State (MkDepGEnv a) Bool
dgeIsWalked a = do
  (MkDepGEnv env) <- get
  return $ a `elem` env

dependsOn :: (Eq a) => DependencyGraph a -> a -> Bool
dependsOn (DependencyTree a adeps) b = a == b || any (`dependsOn` b) adeps
dependsOn (DependencyCycle as asdeps) b = b `elem` as || any (`dependsOn` b) asdeps

mkDependencyGraphS :: (Eq a) => [a] -> (a -> [a]) -> State (MkDepGEnv a) [DependencyGraph a]
mkDependencyGraphS [] _ = return []
mkDependencyGraphS (a : as) depf = do
  walked <- dgeIsWalked a
  if walked
    then do
      mkDependencyGraphS as depf
    else do
      dgeSetWalked a
      next <- mkDependencyGraphS as depf
      let (ad, _) = mkDependencyGraphH depf [] a
      return $
        ad
          : filter
            ( \case
                DependencyCycle as' _ -> not $ any (ad `dependsOn`) as'
                DependencyTree a' _ -> not $ ad `dependsOn` a'
            )
            next

mkDependencyGraphH :: (Eq a) => (a -> [a]) -> PrevPath a -> a -> (DependencyGraph a, Int)
mkDependencyGraphH depf prevPath node =
  if node `elem` prevPath
    then
      let dcycle = takeWhile (/= node) prevPath
       in (DependencyCycle [] [], length dcycle + 1)
    else
      let (prGraph, n) = foldr procNexts (DependencyTree node [], 0) nexts
       in case prGraph of
            cy@(DependencyCycle _ _) -> (cy, n - 1)
            a -> (a, 0)
  where
    nexts = map (mkDependencyGraphH depf (node : prevPath)) (depf node)
    procNexts :: Eq a => (DependencyGraph a, Int) -> (DependencyGraph a, Int) -> (DependencyGraph a, Int)
    procNexts (dta@(DependencyTree _ _), _) (DependencyTree node' ndeps, _) = (DependencyTree node' (dta -: ndeps), 0)
    procNexts (dta@(DependencyTree _ _), _) (DependencyCycle nodes' ndeps, n) = (DependencyCycle nodes' (dta -: ndeps), n)
    procNexts (dca@(DependencyCycle _ _), 0) (DependencyTree node' ndeps, _) = (DependencyTree node' (dca -: ndeps), 0)
    procNexts (DependencyCycle nodes' cdeps, n) (DependencyTree node' ndeps, _) = (DependencyCycle (node' : nodes') (ndeps +-+ cdeps), n)
    procNexts (dca@(DependencyCycle _ _), 0) (DependencyCycle nodes' ndeps, n) = (DependencyCycle nodes' (dca -: ndeps), n)
    procNexts (DependencyCycle anodes' adeps, an) (DependencyCycle bnodes' bdeps, bn) =
      (DependencyCycle (anodes' +-+ bnodes') (adeps +-+ bdeps), max an bn)

dgFoldr :: ([a] -> b -> b) -> (a -> b -> b) -> b -> DependencyGraph a -> b
dgFoldr fc ft b (DependencyTree a adeps) = ft a (foldr (flip $ dgFoldr fc ft) b adeps)
dgFoldr fc ft b (DependencyCycle as asdeps) = fc as (foldr (flip $ dgFoldr fc ft) b asdeps)
