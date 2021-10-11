{-# LANGUAGE BlockArguments, LambdaCase #-}

module Main where

import Control.Concurrent.MVar(MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent(forkIO, forkFinally)
import Control.Monad(void)
import Data.Foldable(traverse_, for_)

readVecs :: String -> [(Int, Int)]
readVecs text = zip a b
  where
    [a, b] = (read <$>) . words  <$> lines text

fork :: IO () -> IO (MVar ())
fork th = do
  m <- newEmptyMVar
  _ <- forkFinally th (\_ -> putMVar m ())
  pure m

producer :: MVar (Maybe Int) -> [(Int, Int)] -> IO ()
producer m vs = do
  for_ vs \(a, b) -> putMVar m $ Just $ a * b
  putMVar m Nothing

consumer :: MVar (Maybe Int) -> Int -> IO ()
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
