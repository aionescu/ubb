module KeyGen where

import Control.Monad.Extra(orM)
import Data.Bifunctor(bimap)
import System.Random.Stateful(StatefulGen, uniformRM)
import Data.Bits(bit, shiftL, (.|.))
import System.Random.MWC(withSystemRandomST)

import Math

-- Translated from http://rosettacode.org/wiki/Miller%E2%80%93Rabin_primality_test#Python
-- Also see: https://gist.github.com/Ayrx/5884790

millerRabin :: StatefulGen g m => Int -> Integer -> g -> m Bool
millerRabin k n g
  | n == 1 = pure False
  | n == 2 = pure True
  | even n = pure False
  | k < 9 = pure True
  | otherwise = not <$> orM (replicate k testRound)
  where
    (d, s) = while (even . fst) (bimap (// 2) (+ 1)) (n - 1, 0 :: Integer)
    trialComposite a = pow a d n /= 1 && not (any (\i -> pow a (2^i * d) n == n - 1) [0 .. s - 1])
    testRound = trialComposite <$> uniformRM (2, n - 1) g

randomBits :: StatefulGen g m => Int -> g -> m Integer
randomBits n = uniformRM (bit (n - 1), bit n - 1)

random3 :: StatefulGen g m => Int -> g -> m Integer
random3 n g = (.|. 3) . (`shiftL` 2) <$> randomBits (n - 2) g

randomPrime :: StatefulGen g m => Int -> Int -> g -> m Integer
randomPrime n k g = do
  p <- random3 n g
  millerRabin k p g >>= \case
    True -> pure p
    False -> randomPrime n k g

genKey :: StatefulGen g m => Int -> Int -> g -> m (Integer, Integer)
genKey n k g = (,) <$> p <*> p
  where
    p = randomPrime n k g

genKeyIO :: Int -> Int -> IO (Integer, Integer)
genKeyIO n k = withSystemRandomST $ genKey n k
