from typing import Callable, Dict, List, Optional, Set
from math import sqrt
from queue import PriorityQueue

from Map import Map, Point

Path = Optional[List[Point]]
Search = Callable[['Map', Point, Point], Path]

def neighbors(p: Point) -> List[Point]:
  (x, y) = p
  return [(x - 1, y), (x, y - 1), (x + 1, y), (x, y + 1)]

def manhattan(a: Point, b: Point) -> int:
  return abs(b[0] - a[0]) + abs(b[1] - a[1])

def euclidean(a: Point, b: Point) -> float:
  x_diff = b[0] - a[0]
  y_diff = b[1] - a[1]
  return sqrt(x_diff * x_diff + y_diff * y_diff)

def unroll_path(prev: Dict[Point, Point], crr: Point) -> List[Point]:
  path = [crr]

  while crr in prev:
    crr = prev[crr]
    path.append(crr)

  path.reverse()
  return path

def search_astar(map: Map, start: Point, end: Point) -> Path:
  prev: Dict[Point, Point] = {}
  g = { start: 0 }

  queue: PriorityQueue = PriorityQueue()
  queue.put((manhattan(start, end), start))

  while not queue.empty():
    (_, crr) = queue.get()

    if crr == end:
      return unroll_path(prev, crr)

    for nb in neighbors(crr):
      new_g = g[crr] + 1

      if nb not in g or new_g < g[nb]:
        g[nb] = new_g

        prev[nb] = crr
        queue.put((new_g + manhattan(nb, end), nb))

  return None

def search_greedy(map: Map, start: Point, end: Point) -> Path:
  prev: Dict[Point, Point] = {}
  seen = set()

  queue: PriorityQueue = PriorityQueue()
  queue.put((manhattan(start, end), start))

  while not queue.empty():
    (_, crr) = queue.get()
    seen.add(crr)

    if crr == end:
      return unroll_path(prev, crr)

    for nb in neighbors(crr):
      if nb not in seen:
        prev[nb] = crr
        queue.put((manhattan(nb, end), nb))

  return None

search_algorithms: Dict[str, Search] = {
  "astar": search_astar,
  "greedy": search_greedy
}