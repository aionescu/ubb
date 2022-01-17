import pickle
import numpy as np
from typing import List, Tuple
from random import shuffle

Point = Tuple[int, int]

class Map():
  def __init__(self, height: int = 20, width: int = 20) -> None:
    self.__height = height
    self.__width = width
    self.__surface = np.zeros((height, width))

  def __getitem__(self, pos: Point) -> int:
    return self.__surface[pos] # type: ignore

  def __setitem__(self, pos: Point, val: int) -> None:
    self.__surface[pos] = val

  @property
  def height(self) -> int:
    return self.__height

  @property
  def width(self) -> int:
    return self.__width

  def positions(self) -> List[Point]:
    return [(x, y) for x in range(self.__height) for y in range(self.__width)]

  def neighbors(self, p: Point) -> List[Point]:
    def valid(x: int, y: int) -> bool:
      return (
        x >= 0 and x < self.__height
        and y >= 0 and y < self.__width
        and self[(x, y)] == 0
      )

    (x, y) = p
    return list(filter(lambda p: valid(*p), [(x - 1, y), (x, y - 1), (x + 1, y), (x, y + 1)]))

  def fill_random(self, fill: float = 0.2) -> None:
    shuffle_vec = self.positions()
    shuffle(shuffle_vec)

    size = self.__height * self.__width
    wall_count = int(size * fill)

    for i in range(size):
      self[shuffle_vec[i]] = int(i < wall_count)

  def empty_points(self) -> Tuple[Point, Point]:
    shuffle_vec = list(filter(lambda p: self[p] == 0, self.positions()))
    shuffle(shuffle_vec)

    return (shuffle_vec[0], shuffle_vec[1])

  def save(self, path: str) -> None:
    with open(path, 'wb') as f:
      pickle.dump(self, f)

  @staticmethod
  def load(path: str) -> 'Map':
    with open(path, "rb") as f:
      return pickle.load(f) # type: ignore
