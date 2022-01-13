module Encrypt where

import Data.Binary
import Data.List.Split(chunksOf)

import Math
import Data.Maybe (mapMaybe)
import Data.Bits (shiftL, shiftR)
import Data.Foldable (foldl')
import Data.List (unfoldr)

type Byte = Word8
type Bytes = [Byte]
type Block = [Byte]

-- Assuming 256-bit p and q

blockSize :: Int
blockSize = 61

redundancySize :: Int
redundancySize = 2

paddedSize :: Int
paddedSize = blockSize + redundancySize

cipherSize :: Int
cipherSize = 64

toNumber :: Bytes -> Integer
toNumber = foldl' (\i b -> (i `shiftL` 8) + fromIntegral b) 0

ofNumber :: Int -> Integer -> Bytes
ofNumber n i = replicate (n - length bs) 0 <> bs
  where
    bs = reverse $ unfoldr f i

    f 0 = Nothing
    f s = Just (fromIntegral $ s % 256, s `shiftR` 8)

chunks :: Int -> a -> [a] -> [[a]]
chunks _ _ [] = []
chunks k d s
  | length s < k = [s <> replicate (k - length s) d]
  | otherwise = take k s : chunks k d (drop k s)

toBlocks :: Bytes -> [Block]
toBlocks bs = chunks blockSize 0 (padding : bs)
  where
    extra = (length bs + 1) % blockSize
    padding
      | extra == 0 = 0
      | otherwise = fromIntegral $ blockSize - extra

ofBlocks :: Bytes -> [Block]
ofBlocks bs
  | length bs % cipherSize /= 0 = error "ofBlocks: Invalid block size"
  | otherwise = chunksOf cipherSize bs

removePadding :: Bytes -> Bytes
removePadding [] = error "removePadding: Empty list"
removePadding (padding : bs) = reverse $ drop (fromIntegral padding) $ reverse bs

addRedundancy :: Block -> Block
addRedundancy b = take redundancySize b <> b

checkRedundancy :: Block -> Maybe Block
checkRedundancy b
  | b0 == take redundancySize b' = Just b'
  | otherwise = Nothing
  where
    (b0, b') = splitAt redundancySize b

encryptBlock :: Integer -> Block -> Block
encryptBlock n b = ofNumber cipherSize $ encryptRaw n $ toNumber $ addRedundancy b

decryptBlock :: Integer -> Integer -> Block -> Block
decryptBlock p q b =
  case mapMaybe (checkRedundancy . ofNumber paddedSize) $ decryptRaw p q $ toNumber b of
    [] -> error "decryptBlock: No square roots"
    [m] -> m
    _ -> error "decryptBlock: Multiple square roots"

encryptBytes :: Integer -> Bytes -> Bytes
encryptBytes n bs = foldMap (encryptBlock n) $ toBlocks bs

decryptBytes :: Integer -> Integer -> Bytes -> Bytes
decryptBytes p q bs = removePadding $ foldMap (decryptBlock p q) $ ofBlocks bs
