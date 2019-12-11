from enum import Enum
from typing import Iterable

class Cell(Enum):
  Empty = 0
  P1 = 1
  P2 = 2

  @property
  def flip(self) -> Cell:
    if self == Cell.Empty:
      return self
    elif self == Cell.P1:
      return Cell.P2
    else:
      return Cell.P1

  def __str__(self) -> str:
    if self == Cell.Empty:
      return '.'
    elif self == Cell.P1:
      return '1'
    else:
      return '2'

class Board:
  def __init__(self) -> None:
    self.__width = 7
    self.__height = 6
    self.__board = [[Cell.Empty for y in range(self.__width)] for x in range(self.__height)]

  @property
  def width(self) -> int:
    return self.__width

  @property
  def height(self) -> int:
    return self.__height

  def is_in(self, x: int, y: int) -> bool:
    return (x >= 0 and x < self.height
      and y >= 0 and y < self.width)

  def get(self, x: int, y: int) -> Cell:
    return self.__board[x][y]

  def set(self, x: int, y: int, val: Cell) -> None:
    self.__board[x][y] = val
    
  def yield_all(self) -> Iterable[Cell]:
    for row in self.__board:
      for cell in row:
        yield cell

  def __str__(self) -> str:
    buf = ""
    
    for row in self.__board:
      for cell in row:
        buf = buf + str(cell) + " "
      buf = buf + "\n"

    return buf