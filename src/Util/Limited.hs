{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
module Util.Limited
  ( Limited,
    Limit (..),
    runLimited,
    decLimit,
    limitedTests,
  )
where

import Control.Monad
import Data.Maybe

data Limit = Unlimited | Limit Int

newtype Limited a = Limited (Limit -> (Maybe a, Limit))

instance Functor Limited where
  fmap f (Limited g) =
    Limited
      ( \l ->
          let (aM, lim) = g l
           in if isLimitReached lim
                then (Nothing, lim)
                else (fmap f aM, lim)
      )

instance Applicative Limited where
  pure a = Limited $ \l -> (Just a, l)
  (Limited f) <*> (Limited g) = Limited $ \l ->
    let (fM, fl) = f l
     in case fM of
          Nothing -> (Nothing, fl)
          Just _ | isLimitReached fl -> (Nothing, fl)
          Just f' -> let (gM, gl) = g fl in (fmap f' gM, gl)

instance Monad Limited where
  return = pure
  (Limited f) >>= g = Limited $ \l ->
    let (fM, fl) = f l
     in case fM of
          Nothing -> (Nothing, fl)
          Just _ | isLimitReached fl -> (Nothing, fl)
          Just f' -> let (Limited ab) = g f' in ab fl

decLimit :: Limited ()
decLimit = Limited $ \l -> (Just (), decHelper l)
  where
    decHelper :: Limit -> Limit
    decHelper Unlimited = Unlimited
    decHelper (Limit i) = Limit (i - 1)

isLimitReached :: Limit -> Bool
isLimitReached Unlimited = False
isLimitReached (Limit i) = i <= 0

runLimited :: Limit -> Limited a -> Maybe a
runLimited lim (Limited f) = fst $ f lim

makeLimited :: (a -> b) -> a -> Limited b
makeLimited f a = Limited $ \l -> (Just $ f a, l)

(!!!) :: [a] -> Int -> Limited (Maybe a)
(!!!) [] _ = return Nothing
(!!!) (x : _) 0 = return $ Just x
(!!!) (_ : xs) n = decLimit >> xs !!! (n - 1)

limitedTests :: [Bool]
limitedTests =
  [ runLimited (Limit 10) (foldM (\n _ -> incL n) 0 [1 .. 5]) == Just 5,
    runLimited (Limit 10) (foldM (\n _ -> incL n) 0 [1 .. 10]) == Just 10,
    runLimited (Limit 10) (isPrimeL 5) == Just True,
    isNothing (runLimited (Limit 10) (primes !!! 80)),
    runLimited (Limit 80) (primes !!! 79) == Just (Just 409)
  ]

incL :: Int -> Limited Int
incL = makeLimited (+ 1)

isPrimeL :: Int -> Limited Bool
isPrimeL n = not <$> foldM (\is d -> (|| is) <$> checkDivisibleL n d) False [2 .. n - 1]

checkDivisibleL :: Int -> Int -> Limited Bool
checkDivisibleL n d = makeLimited (== 0) (n `mod` d)

primes :: [Integer]
primes = filter isPrime [2 ..]

isPrime :: Integer -> Bool
isPrime n = all (\m -> n `mod` m /= 0) [2 .. n - 1]
