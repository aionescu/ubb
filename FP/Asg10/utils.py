from typing import Callable, Iterable, Set, TypeVar

T = TypeVar('T') 

def all_f(predicate: Callable[[T], bool], iterable: Iterable[T]) -> bool:
  return all(map(predicate, iterable))
  
def nub(iterable: Iterable[T]) -> Iterable[T]:
  seen: Set[T] = set()
  for item in iterable:
    if item not in seen:
      seen.add(item)
      yield item

def length(i: Iterable[T]) -> int:
  return sum(1 for _ in i)
