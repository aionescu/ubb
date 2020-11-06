unique :: Eq a => [a] -> [a]
unique [] = []
unique (a : a' : as) | a == a' = unique (a' : as)
unique (a : as) = a : unique as

merge :: Ord a => [a] -> [a] -> [a]
merge [] l = l
merge l [] = l
merge (a : as) (b : bs)
  | a < b = a : merge as (b : bs)
  | otherwise = b : merge (a : as) bs

mergeUnique :: Ord a => [a] -> [a] -> [a]
mergeUnique as bs = unique $ merge as bs

data E a = N a | L [a]
  deriving Show

mergeLs :: Ord a => [E a] -> [a]
mergeLs [] = []
mergeLs (N _ : es) = mergeLs es
mergeLs (L as : es) = mergeUnique as (mergeLs es)
