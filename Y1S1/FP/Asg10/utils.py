from typing import Callable, Iterable, Set, TypeVar

T = TypeVar('T') 

# all_f :: (a -> Bool) -> [a] -> Bool
# Function that returns true if all elements in the sequence satisfy the specified predicate
# Input: predicate - The predicate function, iterable - The iterable to search
# Output: True if all elements satisfy the predicate, otherwise False.
def all_f(predicate: Callable[[T], bool], iterable: Iterable[T]) -> bool:
  return all(map(predicate, iterable))
  
# nub :: [a] -> [a]
# Function that returns an iterable containing the first apparition of each element from the source iterable
# Input: iterable - The iterable to filter
# Output: An iterable containing the unique elements of the specified iterable
def nub(iterable: Iterable[T]) -> Iterable[T]:
  seen: Set[T] = set()
  for item in iterable:
    if item not in seen:
      seen.add(item)
      yield item

# length :: [a] -> Int
# Function that returns the length of the specified iterable
# Input: iterable - The iterable to count the length of
# Output: The length of the specified iterable
def length(iterable: Iterable[T]) -> int:
  return sum(1 for _ in iterable)