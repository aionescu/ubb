from typing import Any, Callable, Dict, Generic, Iterable, Iterator, List, Tuple, TypeVar
from random import randint

T = TypeVar("T")

def id(x: T) -> T:
  return x

def gnome_sort(l: List[T], key: Callable[[T], Any] = id) -> None:
  i = 0
  ln = len(l)

  while i < ln:
    if i == 0 or (key(l[i]) >= key(l[i - 1])):
      i += 1
    else:
      l[i], l[i - 1] = l[i - 1], l[i]
      i -= 1

def shuffle(l: List[T]) -> None:
  for i in range(len(l) - 1, 0, -1): 
    j = randint(0, i - 1) 

    l[i], l[j] = l[j], l[i] 

def is_sorted(l: List[T], key: Callable[[T], Any] = id) -> bool:
  for i in range(0, len(l) - 1):
    if key(l[i]) > key(l[i + 1]):
      return False

  return True

def bogo_sort(l: List[T], key: Callable[[T], Any] = id) -> None:
  while not is_sorted(l, key):
    shuffle(l)

K = TypeVar("K")
V = TypeVar("V")

class MapIter(Iterator[K]):
  def __init__(self, iter: Iterator[K]):
    self.__iter = iter

  def __next__(self) -> K:
    return self.__iter.__next__()

class Map(Generic[K, V]):
  def __init__(self, dict: Dict[K, V] = {}) -> None:
    self.__dict = dict

  @property
  def dict(self) -> Dict[K, V]:
    return self.__dict

  def __getitem__(self, k: K) -> V:
    return self.__dict[k]

  def __setitem__(self, k: K, v: V) -> None:
    self.__dict[k] = v

  def __delitem__(self, k: K) -> None:
    del self.__dict[k]

  def __len__(self) -> int:
    return len(self.__dict)

  def __iter__(self) -> Iterator[K]:
    return MapIter(self.__dict.__iter__())

  @property
  def items(self) -> Iterable[Tuple[K, V]]:
    return self.__dict.items()

  @property
  def keys(self) -> Iterable[K]:
    return self.__dict.keys()

  @property
  def values(self) -> Iterable[V]:
    return self.__dict.values()

  def filter_items(self, predicate: Callable[[Tuple[K, V]], bool]) -> Iterable[Tuple[K, V]]:
    return filter(predicate, self.items)

  def filter_keys(self, predicate: Callable[[K], bool]) -> Iterable[K]:
    return filter(predicate, self.keys)

  def filter_values(self, predicate: Callable[[V], bool]) -> Iterable[V]:
    return filter(predicate, self.values)