from typing import Callable, Iterable, List, Optional, Tuple
from enum import Enum
from utils import all_f, nub, length
from board import Board, Cell, Point

class Game:
  def __init__(self) -> None:
    self.__board = Board()
    self.__next_p = Cell.P1
    self.__state: Optional[Cell] = None

  def is_full(self) -> bool:
    return all(map(lambda x: x != Cell.Empty, self.__board.yield_all()))

  def make_move(self, column: int) -> bool:
    column = column - 1

    if column < 0 or column >= self.__board.width:
      return False

    if self.__state is not None:
      return False

    x = self.__board.height - 1
    y = column

    while x >= 0 and self.__board.get((x, y)) != Cell.Empty:
      x -= 1

    if x == -1:
      return False

    self.__board.set((x, y), self.__next_p)

    if self.is_winning_move(x, y):
      self.__state = self.__next_p
    elif self.__board.is_full():
      self.__state = Cell.Empty
    else:
      self.__next_p = self.__next_p.flip

    return True

  def ended(self) -> bool:
    return self.__state is not None

  def is_winning_move(self, x: int, y: int) -> bool:
    return (self.win_horizontal(x, y)
      or self.win_vertical(x, y)
      or self.win_diag1(x, y)
      or self.win_diag2(x, y))

  def win_slice(self, f: Callable[[Point], Point], slice: Iterable[Point]) -> bool:
    def go(steps: int, f: Callable[[Point], Point], slice: Iterable[Point]) -> bool:
      if steps == 0:
        return False
      
      if not all_f(self.__board.is_in, slice):
        return go(steps - 1, f, map(f, slice))

      cells = nub(map(self.__board.get, slice))

      if length(cells) == 1:
        return True

      return go(steps - 1, f, map(f, slice))

    return go(4, f, slice)

  def win_horizontal(self, x: int, y: int) -> bool:
    slice = [(x, y - 3), (x, y - 2), (x, y - 1), (x, y)]
    def f(p: Point) -> Point:
      x, y = p
      return (x, y + 1)

    return self.win_slice(f, slice)

  def win_vertical(self, x: int, y: int) -> bool:
    slice = [(x - 3, y), (x - 2, y), (x - 1, y), (x, y)]
    def f(p: Point) -> Point:
      x, y = p
      return (x + 1, y)

    return self.win_slice(f, slice)

  def win_diag1(self, x: int, y: int) -> bool:
    slice = [(x - 3, y - 3), (x - 2, y - 2), (x - 1, y - 1), (x, y)]
    def f(p: Point) -> Point:
      x, y = p
      return (x + 1, y + 1)

    return self.win_slice(f, slice)

  def win_diag2(self, x: int, y: int) -> bool:
    slice = [(x + 3, y - 3), (x + 2, y - 2), (x + 1, y - 1), (x, y)]
    def f(p: Point) -> Point:
      x, y = p
      return (x - 1, y + 1)

    return self.win_slice(f, slice)

  def __str__(self) -> str:
    header = ' '.join(map(str, range(1, self.__board.width + 1)))
    board = str(self.__board)

    if self.__state is None:
      text = "It is player " + str(self.__next_p) + "'s turn."
    elif self.__state == Cell.Empty:
      text = "Game ended in a draw."
    else:
      text = "Player " + str(self.__state) + " won."

    return header + "\n\n" + board + "\n" + text