{-# LANGUAGE BlockArguments, LambdaCase #-}

module Main where

import Control.Concurrent(forkFinally, threadDelay)
import Control.Concurrent.MVar(MVar, newEmptyMVar, putMVar, takeMVar)
import Data.Foldable(traverse_, for_)
import Data.List(transpose)
import System.IO (hFlush, stdout, hSetBuffering, BufferMode (LineBuffering))

readVecs :: String -> [[Integer]]
readVecs = transpose . ((read <$>) . words <$>) . lines

print' :: String -> IO ()
print' s = putStrLn s *> hFlush stdout

fork :: IO () -> IO (MVar ())
fork th = do
  m <- newEmptyMVar
  _ <- forkFinally th \_ -> putMVar m ()
  pure m

producer :: MVar (Maybe Integer) -> [[Integer]] -> IO ()
producer m vs = do
  for_ vs \v -> do
    threadDelay 500000
    print' $ "Producer sending: " ++ show v
    putMVar m $ Just $ product v

  putMVar m Nothing

consumer :: MVar (Maybe Integer) -> Integer -> IO ()
consumer m acc =
  takeMVar m >>= \case
    Nothing -> print' $ "Final sum: " ++ show acc
    Just v -> do
      print' $ "Consumer received: " ++ show v
      consumer m $ acc + v

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering

  vs <- readVecs <$> readFile "Vecs.txt"
  m <- newEmptyMVar

  ts <- traverse fork [producer m vs, consumer m 0]
  traverse_ takeMVar ts
