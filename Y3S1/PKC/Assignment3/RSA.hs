{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BlockArguments #-}

import Control.Monad(join)

chunks :: Int -> String -> [String]
chunks _ [] = []
chunks k s
  | length s < k = [s <> replicate (k - length s) '_']
  | otherwise = take k s : chunks k (drop k s)

int :: Char -> Int
int '_' = 0
int c = fromEnum c - fromEnum 'A' + 1

chr :: Int -> Char
chr 0 = '_'
chr i = toEnum (fromEnum 'A' + i - 1)

block :: String -> Int
block s = sum $ zipWith (\i c -> 27 ^ i * int c) [0 :: Integer ..] (reverse s)

unblock :: Int -> Int -> String
unblock l = go (l - 1)
  where
    go _ 0 = []
    go i n = chr r : go (i - 1) q
      where
        (r, q) = n `quotRem` (27 ^ i)

getE :: Int -> Int
getE φ = head $ filter (\x -> gcd x φ == 1) [2 .. φ - 1]

getD :: Int -> Int -> Int
getD φ e = head $ filter (\x -> x * e `rem` φ == 1) [1 .. φ - 1]

getKeys :: Int -> (Int, Int)
getKeys φ = (e, getD φ e)
  where
    e = getE φ

encrypt :: Int -> Int -> Int -> Int
encrypt n e m = fromInteger $ toInteger m ^ toInteger e `rem` toInteger n

decrypt :: Int -> Int -> Int -> Int
decrypt n d c = fromInteger $ toInteger c ^ toInteger d `rem` toInteger n

main :: IO ()
main = do
  let
    p = 61
    q = 67

    n = p * q
    φ = (p - 1) * (q - 1)

    k = 2
    l = 3

    (e, d) = getKeys φ

    ciphertext = "BMOBZEAPV"

    blocks = chunks l ciphertext
    nums = block <$> blocks

    decrypted = decrypt n d <$> nums
    dBlocks = unblock k <$> decrypted

  putStrLn $ "n = " <> show n
  putStrLn $ "φ(n) = " <> show φ
  putStrLn $ "e = " <> show e
  putStrLn $ "d = " <> show d
  putStrLn ""

  print blocks
  print nums
  putStrLn ""

  print decrypted
  print dBlocks
  putStrLn ""

  putStrLn $ join dBlocks
