from typing import Iterable, Tuple
from utils import all_f
from enum import Enum

# Type that represents coordinates
Point = Tuple[int, int]

# Enum class that represents a single cell of the board
# Cases:
#   - Empty: No player has a piece on this cell
#   - P1: Player 1 has a piece on this cell
#   - P2: Player 2 has a piece on this cell
class Cell(Enum):
  Empty = 0
  P1 = 1
  P2 = 2

  # Flips the current cell, return P1 for P2 and vice versa
  @property
  def flip(self) -> 'Cell':
    if self == Cell.P1:
      return Cell.P2
    elif self == Cell.P2:
      return Cell.P1
    else:
      return self

  def __str__(self) -> str:
    if self == Cell.Empty:
      return '.'
    elif self == Cell.P1:
      return '1'
    else:
      return '2'

# Class that represents the game's board
class Board:
  def __init__(self) -> None:
    self.__width = 7
    self.__height = 6
    self.__board = [[Cell.Empty for y in range(self.__width)] for x in range(self.__height)]

  # Gets the width of the board
  @property
  def width(self) -> int:
    return self.__width

  # Gets the height of the board
  @property
  def height(self) -> int:
    return self.__height

  # Checks whether the specified point is within the board's bounds
  # Input: p - The point to check
  # Output: True if p is inside the board, otherwise False
  def is_in(self, p: Point) -> bool:
    x, y = p

    return (x >= 0 and x < self.height
      and y >= 0 and y < self.width)

  # Gets the value of the cell with the specified coordinate
  # Input: p - The coordinate to get the value of
  # Output: The value of the cell at the specified coordonate
  # Raises: IndexError if the specified coordinate is not inside the board
  def get(self, p: Point) -> Cell:
    x, y = p
    return self.__board[x][y]

  # Sets the value of the cell with the specified coordonate to the specified value
  # Input: p - The coordonate to set, val - The value to set
  # Output: None
  # Raises: IndexError if the specified coordinate is not inside the board
  def set(self, p: Point, val: Cell) -> None:
    x, y = p
    self.__board[x][y] = val
    
  def yield_all(self) -> Iterable[Cell]:
    for row in self.__board:
      for cell in row:
        yield cell

  def is_full(self) -> bool:
    def not_empty(cell: Cell) -> bool:
      return cell != Cell.Empty

    return all_f(not_empty, self.yield_all())

  def __str__(self) -> str:
    buf = ""
    
    for row in self.__board:
      for cell in row:
        buf = buf + str(cell) + " "
      buf = buf + "\n"

    return buf