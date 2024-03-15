module Util (
  sinkL,
  sinkR,
  State (..),
  Writer (..),
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
  forgivingZipWithMM,
  forgivingZipWithME,
  (<<$>>),
  (<<*>>),
  (>$<),
  (>*<),
  fstMap,
  sndMap,
  leftMap,
  except,
  maximumM,
  Default,
  def
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

newtype Writer w a = Writer {runWriter :: (a, w)}

instance (Semigroup a, Semigroup w) => Semigroup (Writer w a) where
  (Writer (a1, w1)) <> (Writer (a2, w2)) = Writer (a1 <> a2, w1 <> w2)

instance (Monoid a, Monoid w) => Monoid (Writer w a) where
  mempty = Writer (mempty, mempty)

instance Functor (Writer w) where
  fmap f (Writer (a, w)) = Writer (f a, w)

instance (Monoid w) => Applicative (Writer w) where
  pure a = Writer (a, mempty)
  (Writer (fa1, w1)) <*> (Writer (a2, w2)) = Writer (fa1 a2, w1 <> w2)

instance (Monoid w) => Monad (Writer w) where
  (Writer (a1, w1)) >>= f = let Writer (a2, w2) = f a1 in Writer (a2, w1 <> w2)

class Default a where
    def :: a

instance (Default a, Default b) => Default (a, b) where
    def = (def, def)

instance Default Integer where
    def = 0

instance Default Int where
    def = 0

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
forgivingZipWithM f (x : xs) (y : ys) =
  (:)
    <$> f x y
    <*> forgivingZipWithM f xs ys

forgivingZipWithME ::
  (Applicative m) => (a -> a -> m (Either e a)) -> [a] -> [a] -> m [Either e a]
forgivingZipWithME _ [] r = pure (Right <$> r)
forgivingZipWithME _ r [] = pure (Right <$> r)
forgivingZipWithME f (x : xs) (y : ys) =
  (:)
    <$> f x y
    <*> forgivingZipWithME f xs ys

forgivingZipWithMM ::
  (Applicative m1, Applicative m2) =>
  (a -> a -> m1 (m2 a)) ->
  [a] ->
  [a] ->
  m1 (m2 [a])
forgivingZipWithMM _ [] bs = pure $ pure bs
forgivingZipWithMM _ as [] = pure $ pure as
forgivingZipWithMM f (a : as) (b : bs) = (:) <<$>> f a b <<*>> forgivingZipWithMM f as bs

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

(<<*>>) ::
  (Applicative m1, Applicative m2) =>
  m1 (m2 (a -> b)) ->
  m1 (m2 a) ->
  m1 (m2 b)
(<<*>>) = liftA2 (<*>)

infixl 4 <<$>>
infixl 3 <<*>>

fstMap :: (a -> b) -> (a, c) -> (b, c)
fstMap f (a, c) = (f a, c)

sndMap :: (a -> b) -> (c, a) -> (c, b)
sndMap f (c, a) = (c, f a)

leftMap :: (a -> b) -> Either a c -> Either b c
leftMap _ (Right a) = Right a
leftMap f (Left a) = Left (f a)

except :: (Eq a) => [a] -> [a] -> [a]
except as bs = filter (`notElem` bs) as

infixl 4 `except`

(>$<) :: (Functor f) => f a -> (a -> b) -> f b
(>$<) = flip (<$>)

(>*<) :: (Applicative m) => m a -> m (a -> b) -> m b
(>*<) = flip (<*>)

infixl 4 >$<
infixl 3 >*<

maximumM :: (Foldable f, Ord a) => f a -> Maybe a
maximumM =
  foldr
    ( \v acc -> case acc of
        Nothing -> Just v
        Just v' -> if v > v' then Just v else Just v'
    )
    Nothing
