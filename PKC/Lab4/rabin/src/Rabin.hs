module Rabin where

import Data.Binary
import Data.ByteString.Lazy qualified as B
import Data.ByteString.Lazy.UTF8 qualified as B
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
blockSize = 32

redundancySize :: Int
redundancySize = 31

paddedSize :: Int
paddedSize = 64

ofString :: String -> Bytes
ofString = B.unpack . B.fromString

toString :: Bytes -> String
toString = B.toString . B.pack

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
    extra = length bs % blockSize
    padding
      | extra == 0 = 0
      | otherwise = fromIntegral $ blockSize - extra

ofBlocks :: Bytes -> [Block]
ofBlocks bs
  | length bs % paddedSize /= 0 = error "ofBlocks: Invalid block size"
  | otherwise = chunksOf paddedSize bs

removePadding :: Bytes -> Bytes
removePadding [] = error "removePadding: Empty list"
removePadding (padding : bs) = reverse $ drop (fromIntegral padding - 1) $ reverse bs

addRedundancy :: Block -> Block
addRedundancy b = 0 : take redundancySize b <> b

checkRedundancy :: Block -> Maybe Block
checkRedundancy b
  | b0 == take redundancySize b' = Just b'
  | otherwise = Nothing
  where
    (b0, b') = splitAt redundancySize $ tail b

encryptBlock :: Integer -> Block -> Block
encryptBlock n b = ofNumber paddedSize $ encryptRaw n $ toNumber $ addRedundancy b

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

encryptString :: Integer -> String -> Bytes
encryptString n = encryptBytes n . ofString

decryptString :: Integer -> Integer -> Bytes -> String
decryptString p q = toString . decryptBytes p q
