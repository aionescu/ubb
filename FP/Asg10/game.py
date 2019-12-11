from typing import Optional, Callable, Iterable, TypeVar
from enum import Enum
from exn import InvalidMoveError
from board import Cell, Board

T = TypeVar('T') 

def all_f(predicate: Callable[[T], bool], iterable: Iterable[T]) -> bool:
  return all(map(predicate, iterable))

class Winner(Enum):
  NoneYet = 0
  Draw = 1
  P1Won = 2
  P2Won = 3

class Game:
  def __init__(self) -> None:
    self.__board = Board()
    self.__next_p = Cell.P1

  def is_full(self) -> bool:
    return all(map(lambda x: x != Cell.Empty, self.__board.yield_all()))

  def make_move(self, column: int) -> Winner:
    x = self.__board.height - 1
    y = column

    while x >= 0 and self.__board.get(x, y) != Cell.Empty:
      x -= 1
      
    if x == -1:
      raise InvalidMoveError()
    else:
      self.__board.set(x, y, self.__next_p)
      self.__next_p = self.__next_p.flip

    return self.winner_of(x, y)

  def winner_of(self, x: int, y: int) -> Winner:
    hor = [(x, y - 3), (x, y - 2), (x, y - 1), (x, y)]
    ver = [(x - 3, y), (x - 2, y), (x - 1, y), (x, y)]
    diag1 = [(x - 3, y - 3), (x - 2, y - 2), (x - 1, y - 1), (x, y)]
    diag2 = [(x - 3, y - 3), (x - 2, y - 2), (x - 1, y - 1), (x, y)]