module Util
  ( sinkL,
    sinkR,
    State (..),
    get,
    put,
  )
where

import Control.Applicative

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
