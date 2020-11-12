{-# LANGUAGE NoImplicitPrelude #-}
import Prelude hiding (max, lcm)

-- a)

max x y
  | x > y = x
  | otherwise = y

lcm' x y c
  | c `mod` x == 0, c `mod` y == 0 = c
  | otherwise = lcm' x y (c + 1)

lcm x y = lcm' x y (max x y)

lcmList [] = 0
lcmList [l1] = l1
lcmList (l1 : ls) = lcm l1 (lcmList ls)

-- b)

replace [] e e1 = []
replace (l1 : ls) e e1
  | l1 == e = e1 : replace ls e e1
  | otherwise = l1 : replace ls e e1

main = do
  print $ lcmList [1..4]
  print $ lcmList [1, 10, 20, 30]

  print $ replace [1, 2, 3, 2, 5] 2 6
  print $ replace [2] 2 6
  print $ replace [3] 2 6
  print $ replace [] 2 6
