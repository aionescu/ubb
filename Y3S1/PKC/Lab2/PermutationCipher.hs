{-# LANGUAGE ImportQualifiedPost, RecordWildCards, ScopedTypeVariables, TypeOperators #-}

import Data.Map.Strict(Map)
import Data.Map.Strict qualified as M
import Data.Tuple(swap)
import System.Environment(getArgs)

data a <-> b =
  (:<->)
  { apply :: a -> b
  , unapply :: b -> a
  }

infixr 5 <->
infixr 5 :<->

inv :: a <-> b -> b <-> a
inv (a :<-> b) = b :<-> a

bij :: (Ord a, Ord b) => [(a, b)] -> a <-> b
bij ps = fn ps :<-> fn (swap <$> ps)
  where
    fn :: Ord a => [(a, b)] -> a -> b
    fn = (M.!) . M.fromList

encrypt :: forall a. Int <-> Int -> Int -> a -> [a] -> [a]
encrypt σ m filler plaintext = concat $ applyCipher <$> chunks plaintext
  where
    chunks :: [a] -> [[a]]
    chunks text
      | length text <= m = [take m $ text <> repeat filler]
      | otherwise = take m text : chunks (drop m text)

    applyCipher :: [a] -> [a]
    applyCipher text = lookup . apply σ <$> indices
      where
        indices = [1 .. length text]
        lookup = (text !!) . pred

decrypt :: Int <-> Int -> Int -> a -> [a] -> [a]
decrypt σ = encrypt (inv σ)

main :: IO ()
main = do
  let
    perm = zip [1..] [2, 4, 5, 3, 1]

    m = length perm
    σ = bij perm

    plaintext = "computational"
    encrypted = encrypt σ m '_' plaintext
    decrypted = decrypt σ m '_' encrypted

  putStrLn $ "Plaintext: " <> plaintext
  putStrLn $ "Encrypted: " <> encrypted
  putStrLn $ "Decrypted: " <> decrypted
