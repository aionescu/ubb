from enum import Enum
from random import shuffle, randint
from typing import List, Set, Tuple, Type
import numpy as np
import pickle

Point = Tuple[int, int]

def manhattan(p1: Point, p2: Point) -> int:
  x1, y1 = p1
  x2, y2 = p2
  return abs(x2 - x1) + abs(y2 - y1)

class Dir(Enum):
  UP = 0
  DOWN = 1
  LEFT = 2
  RIGHT = 3

def random_dir() -> Dir:
  return Dir(randint(0, 3))

def move_point(p: Point, d: Dir) -> Point:
  x, y = p

  if d is Dir.UP:
    return x - 1, y
  elif d is Dir.DOWN:
    return x + 1, y
  elif d is Dir.LEFT:
    return x, y - 1
  elif d is Dir.RIGHT:
    return x, y + 1

class Map():
  def __init__(self, width: int = 20, height: int = 20) -> None:
    self.__width = width
    self.__height = height
    self.__surface = np.zeros((width, height))

  @staticmethod
  def randomized(width: int = 20, height: int = 20, fill: float = 0.2) -> 'Map':
    m = Map(width, height)

    shuffle_vec = m.positions()
    shuffle(shuffle_vec)

    size = width * height
    wall_count = int(size * fill)

    for i in range(size):
      m[shuffle_vec[i]] = int(i < wall_count)

    return m

  @property
  def height(self) -> int:
    return self.__height

  @property
  def width(self) -> int:
    return self.__width

  def __getitem__(self, pos: Point) -> int:
    return self.__surface[pos] # type: ignore

  def __setitem__(self, pos: Point, val: int) -> None:
    self.__surface[pos] = val

  def positions(self) -> List[Point]:
    return [(x, y) for x in range(self.__height) for y in range(self.__width)]

  def random_empty_pos(self) -> Point:
    shuffle_vec = list(filter(lambda p: self[p] == 0, self.positions()))
    shuffle(shuffle_vec)

    return shuffle_vec[0]

  def __in_range(self, p: Point) -> bool:
    x, y = p
    return 0 <= x < self.__height and 0 <= y < self.__width

  def is_empty(self, p: Point) -> bool:
    return self.__in_range(p) and self[p] == 0

  def __scan_line(self, p: Point, d: Dir) -> Set[Point]:
    line = set()
    p = move_point(p, d)

    while self.is_empty(p):
      line.add(p)
      p = move_point(p, d)

    return line

  def visible_area_horizontal(self, p: Point) -> Set[Point]:
    return self.__scan_line(p, Dir.LEFT).union(self.__scan_line(p, Dir.RIGHT))

  def visible_area_vertical(self, p: Point) -> Set[Point]:
    return self.__scan_line(p, Dir.UP).union(self.__scan_line(p, Dir.DOWN))

  def visible_area(self, p: Point) -> Set[Point]:
    return { p }.union(
      self.__scan_line(p, Dir.UP),
      self.__scan_line(p, Dir.DOWN),
      self.__scan_line(p, Dir.LEFT),
      self.__scan_line(p, Dir.RIGHT)
    )

  def save(self, path: str) -> None:
    with open(path, "wb") as f:
      pickle.dump(self, f)

  @staticmethod
  def load(path: str) -> 'Map':
    with open(path, "rb") as f:
      map = pickle.load(f)

      if type(map) is not Map:
        raise ValueError(f"Expected to deserialize a 'Map', but deserialized a '{type(map).__name__}''.")

      return map # type: ignore
