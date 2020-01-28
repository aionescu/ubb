import os
from exn import HitAsteroidError, HitEarthError, AlreadyHitError
from game import Game

class Ui:
  def __init__(self):
    self.__game = Game()
    self.__msg = "Please write a command."
    
  def parse_cmd(self, s):
    if s == "exit":
      exit()
    elif s == "cheat":
      self.cheat()
    elif s.startswith("fire"):
      s = s.split()

      coord = self.__game.parse_coord(s[1])
      if coord is None:
        self.__msg = "Invalid coordinate."
        return

      try:
        if self.__game.fire_exn(coord):
          self.__msg = "You hit an alien ship!"
        else:
          self.__msg = "Please write a command."

        self.__game.advance_turn()
      except HitAsteroidError:
        self.__msg = "Cannot hit an asteroid."
        return
      except HitEarthError:
        self.__msg = "Cannot hit the Earth."
        return
      except AlreadyHitError:
        self.__msg = "That cell has already been hit."
        return
    else:
      self.__msg = "Command not recognized."

  def cheat(self):
    self.__game.cheat()

  def advance_turn(self):
    self.draw()

    cmd = input("$ ")
    self.parse_cmd(cmd)

  def draw(self):
    os.system("cls" if os.name == "nt" else "clear")

    print(self.__game)
    print()

    if self.__game.won():
      print("You won.")
      exit()
    elif self.__game.lost():
      print("You lost.")
      exit()

    print(self.__msg)