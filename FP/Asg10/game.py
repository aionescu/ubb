from typing import Callable, Iterable, List, Optional, Set, Tuple, TypeVar
from enum import Enum
from board import Board, Cell, Point

T = TypeVar('T') 

def all_f(predicate: Callable[[T], bool], iterable: Iterable[T]) -> bool:
  return all(map(predicate, iterable))

def any_f(predicate: Callable[[T], bool], iterable: Iterable[T]) -> bool:
  return any(map(predicate, iterable))

def nub(iterable: Iterable[T]) -> Iterable[T]:
  seen: Set[T] = set()
  for item in iterable:
    if item not in seen:
      seen.add(item)
      yield item

def length(l: Iterable[T]) -> int:
  return len(list(l))

class Winner(Enum):
  NoneYet = 0
  Draw = 1
  P1Won = 2
  P2Won = 3

  @staticmethod
  def of_cell(cell: Cell) -> 'Winner':
    if cell == Cell.P1:
      return Winner.P1Won
    elif cell == Cell.P2:
      return Winner.P2Won
    else:
      return Winner.Draw

class Game:
  def __init__(self) -> None:
    self.__board = Board()
    self.__next_p = Cell.P1

  def is_full(self) -> bool:
    return all(map(lambda x: x != Cell.Empty, self.__board.yield_all()))

  def make_move(self, column: int) -> Optional[Winner]:
    x = self.__board.height - 1
    y = column

    while x >= 0 and self.__board.get((x, y)) != Cell.Empty:
      x -= 1
      
    crr_p = self.__next_p

    if x == -1:
      return None
    else:
      self.__board.set((x, y), crr_p)
      self.__next_p = self.__next_p.flip

    if self.is_winning_move(x, y):
      return Winner.of_cell(crr_p)
    else:
      return Winner.NoneYet

  def is_winning_move(self, x: int, y: int) -> bool:
    return (self.win_horizontal(x, y)
      or self.win_vertical(x, y)
      or self.win_diag1(x, y)
      or self.win_diag2(x, y))

  def is_winning_slice(self, iter: Callable[[Point], Point], slice: Iterable[Point]) -> bool:
    def go(steps: int, iter: Callable[[Point], Point], slice: Iterable[Point]) -> bool:
      if steps == 0:
        return False
      
      if not all_f(self.__board.is_in, slice):
        return False

      cells = nub(map(self.__board.get, slice))

      if length(cells) == 1:
        return True

      return go(steps - 1, iter, map(iter, slice))

    return go(4, iter, slice)

  def win_horizontal(self, x: int, y: int) -> bool:
    slice = [(x, y - 3), (x, y - 2), (x, y - 1), (x, y)]
    def iter(p: Point) -> Point:
      x, y = p
      return (x, y + 1)

    return self.is_winning_slice(iter, slice)

  def win_vertical(self, x: int, y: int) -> bool:
    slice = [(x - 3, y), (x - 2, y), (x - 1, y), (x, y)]
    def iter(p: Point) -> Point:
      x, y = p
      return (x + 1, y)

    return self.is_winning_slice(iter, slice)

  def win_diag1(self, x: int, y: int) -> bool:
    slice = [(x - 3, y - 3), (x - 2, y - 2), (x - 1, y - 1), (x, y)]
    def iter(p: Point) -> Point:
      x, y = p
      return (x + 1, y + 1)

    return self.is_winning_slice(iter, slice)

  def win_diag2(self, x: int, y: int) -> bool:
    slice = [(x + 3, y - 3), (x + 2, y - 2), (x + 1, y - 1), (x, y)]
    def iter(p: Point) -> Point:
      x, y = p
      return (x - 1, y + 1)

    return self.is_winning_slice(iter, slice)