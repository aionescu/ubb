from typing import Callable, Dict, List, Optional, Tuple
from math import sqrt
from queue import PriorityQueue, Queue

from Map import Map, Point

Path = Optional[List[Point]]
Search = Callable[['Map', Point, Point], Path]

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
  heuristic = manhattan

  prev: Dict[Point, Point] = {}
  g = { start: 0 }

  queue: PriorityQueue[Tuple[float, Point]] = PriorityQueue()
  queue.put((heuristic(start, end), start))

  while not queue.empty():
    (_, crr) = queue.get()

    if crr == end:
      return unroll_path(prev, crr)

    for nb in map.neighbors(crr):
      new_g = g[crr] + 1

      if nb not in g or new_g < g[nb]:
        g[nb] = new_g

        prev[nb] = crr
        queue.put((new_g + heuristic(nb, end), nb))

  return None

def search_greedy(map: Map, start: Point, end: Point) -> Path:
  heuristic = manhattan

  prev: Dict[Point, Point] = {}
  seen = set()

  queue: PriorityQueue[Tuple[float, Point]] = PriorityQueue()
  queue.put((heuristic(start, end), start))

  while not queue.empty():
    (_, crr) = queue.get()
    seen.add(crr)

    if crr == end:
      return unroll_path(prev, crr)

    for nb in map.neighbors(crr):
      if nb not in seen:
        prev[nb] = crr
        queue.put((heuristic(nb, end), nb))

  return None

def search_lee(map: Map, start: Point, end: Point) -> Path:
  prev: Dict[Point, Point] = {}
  cost = { start: 0 }

  queue: Queue[Point] = Queue()
  queue.put(start)

  while not queue.empty():
    crr = queue.get()
    new_cost = cost[crr] + 1

    for nb in map.neighbors(crr):
      if nb not in cost or new_cost < cost[nb]:
        cost[nb] = new_cost
        queue.put(nb)
        prev[nb] = crr

  if end not in prev:
    return None
  else:
    return unroll_path(prev, end)

search_algorithms: Dict[str, Search] = {
  "astar": search_astar,
  "greedy": search_greedy,
  "lee": search_lee
}
