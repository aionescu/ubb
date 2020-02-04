import os
from random import randint
from board import Cell
from game import Game

class Ui:
  def __init__(self) -> None:
    self.__start()

  def __choose_ai(self) -> None:
    print("Would you like to play against the computer? [y/N]")
    ans = input()

    self.__use_ai = ans == "y"

  def __start(self) -> None:
    self.__game = Game()
    self.__last_invalid = False
    self.__choose_ai()

  def __clear(self) -> None:
    os.system("cls" if os.name == "nt" else "clear")

  def print(self) -> None:
    self.__clear()
    print(self.__game)

  def restart(self) -> None:
    print("Would you like to begin another game? [y/N]")
    ans = input()

    if ans == "y":
      self.__start()
    else:
      exit()
  
  def ai_make_move(self) -> None:
    r = randint(1, self.__game.board_width)

    if not self.__game.make_move(r):
      self.ai_make_move()

  def advance_turn(self) -> None:
    if self.__game.ended():
      self.restart()
      return
    
    if self.__use_ai and self.__game.next_p == Cell.P2:
      self.ai_make_move()
      return

    print("That move was invalid. Please try again." if self.__last_invalid else "Please select which column you wish to place a piece on.")
    inp = input()

    if inp == "exit":
      exit()
      
    try:
      column = int(inp)
    except ValueError:
      print("Column must be a number.")
      return
    
    self.__last_invalid = not self.__game.make_move(column)