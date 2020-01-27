import os
from board import UP, DOWN, LEFT, RIGHT
from game import Game

class Ui:
  def __init__(self, width, height):
    self.__game = Game(width, height)

  def __clear(self):
    os.system("cls" if os.name == "nt" else "clear")

  def initialize(self):
    print("Please enter the positions and orientations of your 3 planes:")

    while True:
      def get_ori(o):
        if o == "up":
          return UP

        if o == "down":
          return DOWN

        if o == "left":
          return LEFT

        if o == "right":
          return RIGHT

        raise ValueError()

      planes = []
      i = 0
      while i < 3:
        while True:
          pos = input()
          ori = input()

          try:
            pos = self.__game.to_coord(pos)
            ori = get_ori(ori)

            planes.append((pos, ori))
            i += 1
          except:
            print("Invalid plane. Please try again.")

          break
      
      if self.__game.initialize_p1(planes):
        break

      print("Those planes were invalid. Please try again.")
      planes.clear()

    self.__game.initialize_p2()

  def make_move(self):
    print("Please make your next move.")

    while True:
      move = input()

      try:
        move = self.__game.to_coord(move)
      except:
        print("Invalid coordinate. Please try again.")
        continue

      if self.__game.make_move_p1(move):
        break

      print("Invalid move. Please try again.")

    self.__game.make_move_p2()

  def check_win(self):
    if self.__game.win_p1():
      self.print()
      print("Player 1 won.")
      exit()
    elif self.__game.win_p2():
      print("Player 2 won.")
      exit()

  def print(self):
    self.__clear()
    print(self.__game)
