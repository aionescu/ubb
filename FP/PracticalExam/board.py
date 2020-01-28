from texttable import Texttable

EMPTY = 0
EARTH = 1
ASTEROID = 2
SHIP = 3
HIT = 4

def str_of_cell(cell, cheat = False):
  if cell == EMPTY:
    return '.'
  elif cell == EARTH:
    return 'E'
  elif cell == ASTEROID:
    return '*'
  elif cell == SHIP:
    return 'X' if cheat else '.'
  elif cell == HIT:
    return '-'
  else:
    raise Exception("Invalid cell.")

MAP_SIZE = 7

class Board:
  def __init__(self):
    self.__board = [[EMPTY for _ in range(MAP_SIZE)] for _ in range(MAP_SIZE)]

  def get(self, p):
    x, y = p
    return self.__board[x][y]

  def set(self, p, val):
    x, y = p
    self.__board[x][y] = val

  def to_str(self, cheat = False):
    tbl = Texttable()
    tbl.header([" ", "A", "B", "C", "D", "E", "F", "G"])

    for x in range(MAP_SIZE):
      num = str(x + 1)
      cells = list(map(lambda x: str_of_cell(x, cheat), self.__board[x]))

      tbl.add_row([num] + cells)
    
    return tbl.draw()