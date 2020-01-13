from board import EMPTY, PLANE_TAIL, PLANE_HEAD

class HitBoard:
  def __init__(self, board):
    self.__board = board
    self.__hits = []

  def is_winner(self):
    def f(c):
      return c == PLANE_HEAD
      
    return len(list(filter(f, map(self.__board.get, self.__hits)))) == 3

  def hit(self, p):
    if p in self.__hits:
      return False

    self.__hits.append(p)
    return True

  def __str__(self):
    header = " ".join(map(str, range(1, self.__board.width + 1)))
    buf = "  " + header + "\n"
    letter = "A"

    def inc_char(c):
      return chr(ord(c) + 1)

    def to_string(cell):
      if cell not in self.__hits:
        return "."

      v = self.__board.get(cell)
      if v == EMPTY:
        return "0"

      if v == PLANE_TAIL:
        return "X"

      return "!"

    for x in range(0, self.__board.height):
      buf += letter + " "
      letter = inc_char(letter)

      for y in range(0, self.__board.width):
        buf += to_string(self.__board.get((x, y))) + " "

      buf += "\n"

    return buf