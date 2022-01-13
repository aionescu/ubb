module Main(main) where

import Data.Bifunctor(bimap)
import Data.ByteString.Lazy qualified as B

import KeyGen
import Rabin
import Opts
import System.Directory.Extra (createDirectoryIfMissing)
import System.FilePath(takeDirectory, (-<.>))

runGenKey :: FilePath -> IO ()
runGenKey path = do
  createDirectoryIfMissing True $ takeDirectory path

  (p, q) <- genKeyIO 256 64
  let
    n = p * q
    privKey = ofNumber 32 p <> ofNumber 32 q
    pubKey = ofNumber 64 n

  B.writeFile (path <> ".key") $ B.pack privKey
  B.writeFile (path <> ".pub") $ B.pack pubKey

runEncrypt :: FilePath -> String -> IO ()
runEncrypt pubKey' msg' = do
  msg <- B.unpack <$> B.readFile msg'
  pubKey <- toNumber . B.unpack <$> B.readFile pubKey'

  B.writeFile (msg' -<.> "cipher") $ B.pack $ encryptBytes pubKey msg

runDecrypt :: FilePath -> String -> IO ()
runDecrypt privKey' cipher' = do
  cipher <- B.unpack <$> B.readFile cipher'
  privKey <- B.unpack <$> B.readFile privKey'

  let (p, q) = bimap toNumber toNumber $ splitAt 32 privKey
  B.writeFile (cipher' -<.> "plain") $ B.pack $ decryptBytes p q cipher

main :: IO ()
main =
  getOpts >>= \case
    GenKey path -> runGenKey path
    Encrypt pubKey msg -> runEncrypt pubKey msg
    Decrypt privKey msg -> runDecrypt privKey msg
