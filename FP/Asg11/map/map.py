from typing import Dict, Iterable, Iterator, Mapping, Tuple, TypeVar

K = TypeVar("K")
V = TypeVar("V")

class MapIter(Iterator[K]):
  def __init__(self, iter: Iterator[K]):
    self.__iter = iter

  def __next__(self) -> K:
    return self.__iter.__next__()

class Map(Mapping[K, V]):
  def __init__(self):
    self.__dict = {}

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