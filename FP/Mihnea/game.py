from random import randint
from board import Board
from hit_board import HitBoard

class Game:
  def __init__(self, width, height):
    self.__board_p1 = Board(width, height)
    self.__board_p2 = Board(width, height)
    self.__hit_board_p1 = HitBoard(self.__board_p2)
    self.__hit_board_p2 = HitBoard(self.__board_p1)

  def to_coord(self, s):
    x = ord(s[0]) - ord('A')
    y = int(s[1:]) - 1

    return (x, y)

  def make_move_p1(self, move):
    return self.__hit_board_p1.hit(move)

  def make_move_p2(self):
    while True:
      rx = randint(0, self.__board_p2.height - 1)
      ry = randint(0, self.__board_p2.width - 1)

      if self.__hit_board_p2.hit((rx, ry)):
        break

  def initialize_p1(self, planes):
    b = self.__board_p1.speculate()

    for (head, orientation) in planes:
      if not b.try_place_plane(head, orientation):
        return False

    for (head, orientation) in planes:
      assert self.__board_p1.try_place_plane(head, orientation)

    return True

  def initialize_p2(self):
    def rand_coord():
      return (randint(0, self.__board_p2.height - 1),
        randint(0, self.__board_p2.width - 1))

    def rand_orientation():
      return randint(0, 3)
    
    def rand_plane():
      return (rand_coord(), rand_orientation())

    while True:
      pl1 = rand_plane()
      pl2 = rand_plane()
      pl3 = rand_plane()
      b = self.__board_p2.speculate()

      if (b.try_place_plane(*pl1)
        and b.try_place_plane(*pl2)
        and b.try_place_plane(*pl3)):

        assert self.__board_p2.try_place_plane(*pl1)
        assert self.__board_p2.try_place_plane(*pl2)
        assert self.__board_p2.try_place_plane(*pl3)
        break

  def win_p1(self):
    return self.__hit_board_p1.is_winner()

  def win_p2(self):
    return self.__hit_board_p2.is_winner()

  def __str__(self):
    buf = ""

    buf += str(self.__board_p1)
    buf += "\n"

    buf += str(self.__hit_board_p1)

    return buf
