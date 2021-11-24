import Data.List(nub, sort, transpose, foldl1')
import System.Environment(getArgs)

-- powerOf n f = (m, p) such that p ^ f * m = n
powerOf :: Int -> Int -> (Int, Int)
powerOf n f
  | n `rem` f == 0 = (+ 1) <$> powerOf (n `quot` f) f
  | otherwise = (n, 0)

-- factors n = [(f1, p1), ..., (fi, pi)] such that f1 ^ p1 * ... * fi ^ pi = n
factors :: Int -> [(Int, Int)]
factors x = go [] [2 .. x `quot` 2] x
  where
    go [] [] _ = [(x, 1)]
    go acc [] _ = reverse acc
    go acc (f : fs) n
      | p > 0 = go ((f, p) : acc) fs n'
      | otherwise = go acc fs n
      where (n', p) = powerOf n f

gcd' :: [Int] -> Int
gcd' = go . nub . sort
  where
    go [] = 1
    go [n] = n
    go (n : ns) = r
      where
        (fs, ps) = unzip $ factors n
        fs' = ps : ((\x -> snd . powerOf x <$> fs) <$> ns)
        r = product $ zipWith (^) fs $ minimum <$> transpose fs'

gcdNaive :: [Int] -> Int
gcdNaive [] = 1
gcdNaive ns = foldl1' gcd ns

main :: IO ()
main = do
  ns <- traverse readIO =<< getArgs

  print $ gcdNaive ns
  print $ gcd' ns
