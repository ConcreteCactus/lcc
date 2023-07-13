module Util
  ( sinkL,
    sinkR,
    State (..),
    get,
    put,
    Subscript (..),
    maybeToEither,
    execState,
    evalState,
  )
where

import Control.Applicative
import Data.Char

sinkL :: Either e a -> Either (Either e f) a
sinkL (Left e) = Left (Left e)
sinkL (Right a) = Right a

sinkR :: Either e a -> Either (Either f e) a
sinkR (Left e) = Left (Right e)
sinkR (Right a) = Right a

newtype State s a = State {runState :: s -> (s, a)}

instance Functor (State s) where
  fmap f (State a) = State $ fmap f . a

instance Applicative (State s) where
  pure a = State (\s -> (s, a))
  (State sab) <*> (State sa) =
    State
      ( \s1 ->
          let (s2, ab) = sab s1
           in let (s3, a) = sa s2 in (s3, ab a)
      )

instance Monad (State s) where
  return = pure
  (State sa) >>= f =
    State
      ( \s1 ->
          let (s2, a) = sa s1
           in let (State sb) = f a in sb s2
      )

get :: State s s
get = State (\s -> (s, s))

put :: s -> State s ()
put ns = State $ const (ns, ())

newtype Subscript = Subscript {unSubscript :: Integer}
  deriving (Eq, Ord)

instance Show Subscript where
  show (Subscript i) = show $ foldr (\c s -> chars !! digitToInt c : s) "" $ show i
    where
      chars :: [Char]
      chars = "₀₁₂₃₄₅₆₇₈₉"

maybeToEither :: Maybe a -> e -> Either e a
maybeToEither Nothing e = Left e
maybeToEither (Just a) _ = Right a

execState :: State s a -> s -> a
execState (State sa) = snd . sa

evalState :: State s a -> s -> s
evalState (State sa) = fst . sa
