from queue import PriorityQueue
from random import shuffle
from timing import timed
from typing import Dict, List, Optional, Tuple
import pickle

Point = Tuple[int, int]

dirs = [(-1, 0), (0, -1), (1, 0), (0, 1)]

def offset_point(p1: Point, p2: Point) -> Point:
  x1, y1 = p1
  x2, y2 = p2
  return (x1 + x2, y1 + y2)

def manhattan(a: Point, b: Point) -> int:
  return abs(b[0] - a[0]) + abs(b[1] - a[1])

def unroll_path(prev: Dict[Point, Point], crr: Point) -> List[Point]:
  path = [crr]

  while crr in prev:
    crr = prev[crr]
    path.append(crr)

  path.reverse()
  return path

class Map:
  def __init__(self, width: int, height: int, surface: List[bool], sensors: List[Point], initial_pos: Point) -> None:
    self.__width = width
    self.__height = height
    self.__surface = surface
    self.__sensors = sensors
    self.__initial_pos = initial_pos

  @staticmethod
  def randomized(width: int = 20, height: int = 20, sensor_count: int = 5, fill: float = 0.2) -> 'Map':
    size = width * height
    surface = [False for _ in range(size)]

    for i in range(int(size * fill)):
      surface[i] = True

    shuffle(surface)

    m = Map(width, height, surface, [], (0, 0))

    pos = m.__empty_positions_shuffled()
    m.__initial_pos = pos.pop(0)
    m.__sensors = pos[:sensor_count]

    return m

  @property
  def height(self) -> int:
    return self.__height

  @property
  def width(self) -> int:
    return self.__width

  @property
  def sensors(self) -> List[Point]:
    return self.__sensors

  @property
  def initial_pos(self) -> Point:
    return self.__initial_pos

  def __getitem__(self, p: Point) -> bool:
    x, y = p
    return self.__surface[x * self.__width + y]

  def __setitem__(self, p: Point, val: bool) -> None:
    x, y = p
    self.__surface[x * self.__width + y] = val

  def positions(self) -> List[Point]:
    return [(x, y) for x in range(self.__height) for y in range(self.__width)]

  def __empty_positions_shuffled(self) -> List[Point]:
    shuffle_vec = list(filter(lambda p: not self[p], self.positions()))
    shuffle(shuffle_vec)

    return shuffle_vec

  def __in_range(self, p: Point) -> bool:
    x, y = p
    return 0 <= x < self.__height and 0 <= y < self.__width

  def is_empty(self, p: Point) -> bool:
    return self.__in_range(p) and not self[p]

  def __scan_line(self, p: Point, dir: Point, energy: int) -> int:
    length = 0
    p = offset_point(p, dir)

    while self.is_empty(p) and length < energy:
      length += 1
      p = offset_point(p, dir)

    return length

  def __scan_line_tiles(self, p: Point, dir: Point, energy: int) -> List[Point]:
    points: List[Point] = []
    p = offset_point(p, dir)

    while self.is_empty(p) and len(points) < energy:
      points.append(p)
      p = offset_point(p, dir)

    return points

  def __visible_area(self, p: Point, energy: int) -> int:
    return sum([self.__scan_line(p, dir, energy) for dir in dirs])

  def visible_area_per_energy(self, p: Point) -> List[int]:
    area = [0]

    last_area = 0
    energy = 1

    while energy < 5:
      new_area = self.__visible_area(p, energy + 1)
      if new_area <= last_area:
        break

      area.append(new_area)
      last_area = new_area
      energy += 1

    return area

  def visible_tiles(self, p: Point, energy: int) -> List[Point]:
    return [point for dir in dirs for point in self.__scan_line_tiles(p, dir, energy)]

  def neighbors(self, p: Point) -> List[Point]:
    return list(filter(self.is_empty, [offset_point(p, dir) for dir in dirs]))

  def get_path(self, start: Point, end: Point) -> Optional[List[Point]]:
    prev: Dict[Point, Point] = { }
    g = { start: 0 }

    queue: PriorityQueue[Tuple[float, Point]] = PriorityQueue()
    queue.put((manhattan(start, end), start))

    while not queue.empty():
      (_, crr) = queue.get()

      if crr == end:
        return unroll_path(prev, crr)

      for nb in self.neighbors(crr):
        new_g = g[crr] + 1

        if nb not in g or new_g < g[nb]:
          g[nb] = new_g

          prev[nb] = crr
          queue.put((new_g + manhattan(nb, end), nb))

    return None

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
