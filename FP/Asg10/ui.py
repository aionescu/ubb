import os
from game import Game

class Ui:
  def __init__(self) -> None:
    self.__game = Game()

  def __clear(self) -> None:
    os.system("cls" if os.name == "nt" else "clear")

  def print(self) -> None:
    self.__clear()
    print(self.__game)

  def restart(self) -> None:
    print("Would you like to begin another game? [y/N]")
    ans = input()

    if ans == "y":
      self.__game = Game()
    else:
      exit()
    
  def advance_turn(self) -> None:
    if self.__game.ended():
      self.restart()
      return

    print("Please select which column you wish to place a piece on.")
    inp = input()

    if inp == "exit":
      exit()
      
    try:
      column = int(inp)
    except ValueError:
      print("Column must be a number.")
      return
    
    self.__game.make_move(column)