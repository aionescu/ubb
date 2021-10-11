{-# LANGUAGE BlockArguments, LambdaCase #-}

module Main where

import Control.Concurrent(forkFinally)
import Control.Concurrent.MVar(MVar, newEmptyMVar, putMVar, takeMVar)
import Data.Foldable(traverse_)
import Data.List(transpose)

readVecs :: String -> [[Integer]]
readVecs = transpose . ((read <$>) . words <$>) . lines

fork :: IO () -> IO (MVar ())
fork th = do
  m <- newEmptyMVar
  _ <- forkFinally th \_ -> putMVar m ()
  pure m

producer :: MVar (Maybe Integer) -> [[Integer]] -> IO ()
producer m vs = do
  traverse_ (putMVar m . Just . product) vs
  putMVar m Nothing

consumer :: MVar (Maybe Integer) -> Integer -> IO ()
consumer m acc =
  takeMVar m >>= \case
    Nothing -> print acc
    Just v -> consumer m $ acc + v

main :: IO ()
main = do
  vs <- readVecs <$> readFile "Vecs.txt"
  m <- newEmptyMVar

  ts <- traverse fork [producer m vs, consumer m 0]
  traverse_ takeMVar ts
