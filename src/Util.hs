module Util (sinkL, sinkR) where

sinkL :: Either e a -> Either (Either e f) a
sinkL (Left e) = Left (Left e)
sinkL (Right a) = Right a

sinkR :: Either e a -> Either (Either f e) a
sinkR (Left e) = Left (Right e)
sinkR (Right a) = Right a
