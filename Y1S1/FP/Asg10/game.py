from typing import Callable, Iterable, List, Optional, Tuple
from enum import Enum
from utils import all_f, nub, length
from board import Board, Cell, Point

# Class that represents a game
class Game:
  def __init__(self) -> None:
    self.__board = Board()
    self.__next_p = Cell.P1
    self.__state: Optional[Cell] = None

  # Property that returns the next player that will make a move
  @property
  def next_p(self) -> Cell:
    return self.__next_p

  # Property that returns the width of the game board
  @property
  def board_width(self) -> int:
    return self.__board.width

  # Makes a move and advances the game by 1 turn
  # Input: column - The column on which to place the next piece
  # Returns: True if the move is valid, otherwise False
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

  # Function that returns whether the game has ended
  def ended(self) -> bool:
    return self.__state is not None

  # Function that returns whether the last move was a winning move
  # Input: x, y - The coordinates of the last move
  # Returns: True if the move is a winning move, otherwise False.
  def is_winning_move(self, x: int, y: int) -> bool:
    return (self.win_horizontal(x, y)
      or self.win_vertical(x, y)
      or self.win_diag1(x, y)
      or self.win_diag2(x, y))

  # Helper function for determining whether a move is a winning move
  def win_slice(self, f: Callable[[Point], Point], slice: List[Point]) -> bool:
    def go(steps: int, f: Callable[[Point], Point], slice: List[Point]) -> bool:
      if steps == 0:
        return False

      if not all_f(self.__board.is_in, slice):
        return go(steps - 1, f, list(map(f, slice)))

      cells = list(nub(map(self.__board.get, slice)))

      if len(cells) == 1 and cells[0] != Cell.Empty:
        return True

      return go(steps - 1, f, list(map(f, slice)))

    return go(4, f, slice)

  # Helper function for determining whether a move is a winning move
  def win_horizontal(self, x: int, y: int) -> bool:
    slice = [(x, y - i) for i in range(3, -1, -1)]
    def f(p: Point) -> Point:
      a, b = p
      return (a, b + 1)

    return self.win_slice(f, slice)

  # Helper function for determining whether a move is a winning move
  def win_vertical(self, x: int, y: int) -> bool:
    slice = [(x - i, y) for i in range(3, -1, -1)]
    def f(p: Point) -> Point:
      a, b = p
      return (a + 1, b)

    return self.win_slice(f, slice)

  # Helper function for determining whether a move is a winning move
  def win_diag1(self, x: int, y: int) -> bool:
    slice = [(x - i, y - i) for i in range(3, -1, -1)]
    def f(p: Point) -> Point:
      a, b = p
      return (a + 1, b + 1)

    return self.win_slice(f, slice)

  # Helper function for determining whether a move is a winning move
  def win_diag2(self, x: int, y: int) -> bool:
    slice = [(x + i, y - i) for i in range(3, -1, -1)]
    def f(p: Point) -> Point:
      a, b = p
      return (a - 1, b + 1)

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