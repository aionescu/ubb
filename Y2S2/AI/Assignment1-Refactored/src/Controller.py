from Env import *
from Domain import *

class Controller:
  def __init__(self, env, map, drone):
    self.__env = env
    self.__map = map
    self.__drone = drone

  def mapImage(self):
    return self.__map.image(*self.__drone.pos())

  def updateState(self):
    self.__drone.moveDFS()
    self.__map.markDetectedWalls(self.__env, *self.__drone.pos())
