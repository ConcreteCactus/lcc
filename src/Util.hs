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
    trim,
    (!!!),
    (+-+),
    (-:),
    forgivingZipWith,
    forgivingZipWithM,
    forgivingZipWithME,
    (<<$>>),
    fstMap,
    sndMap,
    leftMap,
  )
where

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

trim :: String -> String
trim str = reverse $ dropWhile isSpace $ reverse $ dropWhile isSpace str

(!!!) :: [a] -> Int -> Maybe a
(!!!) [] _ = Nothing
(!!!) (x : _) 1 = Just x
(!!!) (_ : xs) n = xs !!! (n - 1)

forgivingZipWith :: (a -> a -> a) -> [a] -> [a] -> [a]
forgivingZipWith _ [] r = r
forgivingZipWith _ r [] = r
forgivingZipWith f (x : xs) (y : ys) = f x y : forgivingZipWith f xs ys

forgivingZipWithM :: (Applicative m) => (a -> a -> m a) -> [a] -> [a] -> m [a]
forgivingZipWithM _ [] r = pure r
forgivingZipWithM _ r [] = pure r
forgivingZipWithM f (x : xs) (y : ys) = (:) <$> f x y <*> forgivingZipWithM f xs ys

forgivingZipWithME :: (Applicative m) => (a -> a -> m (Either e a)) -> [a] -> [a] -> m [Either e a]
forgivingZipWithME _ [] r = pure (Right <$> r)
forgivingZipWithME _ r [] = pure (Right <$> r)
forgivingZipWithME f (x : xs) (y : ys) = (:) <$> f x y <*> forgivingZipWithME f xs ys

(+-+) :: (Eq a) => [a] -> [a] -> [a]
(a : as) +-+ bs
  | a `elem` bs = as +-+ bs
  | otherwise = a : (as +-+ bs)
[] +-+ bs = bs

(-:) :: (Eq a) => a -> [a] -> [a]
a -: as
  | a `elem` as = as
  | otherwise = a : as

(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap

infixl 4 <<$>>

fstMap :: (a -> b) -> (a, c) -> (b, c)
fstMap f (a, c) = (f a, c)

sndMap :: (a -> b) -> (c, a) -> (c, b)
sndMap f (c, a) = (c, f a)

leftMap :: (a -> b) ->  Either a c -> Either b c
leftMap f (Right a) = Right a
leftMap f (Left a) = Left (f a)
