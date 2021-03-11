import pygame
import numpy as np

from Constants import *

class Map():
  def __init__(self):
    self.__n = 20
    self.__m = 20
    self.surface = np.zeros((self.__n, self.__m))
    for i in range(self.__n):
      for j in range(self.__m):
        self.surface[i][j] = -1

  def markDetectedWalls(self, e, x, y):
    #   To DO
    # mark on this map the wals that you detect
    wals = e.readUDMSensors(x, y)
    i = x - 1
    if wals[UP] > 0:
      while ((i>=0) and (i >= x - wals[UP])):
        self.surface[i][y] = 0
        i = i - 1
    if (i>=0):
      self.surface[i][y] = 1

    i = x + 1
    if wals[DOWN] > 0:
      while ((i < self.__n) and (i <= x + wals[DOWN])):
        self.surface[i][y] = 0
        i = i + 1
    if (i < self.__n):
      self.surface[i][y] = 1

    j = y + 1
    if wals[LEFT] > 0:
      while ((j < self.__m) and (j <= y + wals[LEFT])):
        self.surface[x][j] = 0
        j = j + 1
    if (j < self.__m):
      self.surface[x][j] = 1

    j = y - 1
    if wals[RIGHT] > 0:
      while ((j >= 0) and (j >= y - wals[RIGHT])):
        self.surface[x][j] = 0
        j = j - 1
    if (j >= 0):
      self.surface[x][j] = 1

    return None

  def image(self, x, y):
    imagine = pygame.Surface((420,420))
    brick = pygame.Surface((20,20))
    empty = pygame.Surface((20,20))
    empty.fill(WHITE)
    brick.fill(BLACK)
    imagine.fill(GRAYBLUE)

    for i in range(self.__n):
      for j in range(self.__m):
        if (self.surface[i][j] == 1):
          imagine.blit(brick, ( j * 20, i * 20))
        elif (self.surface[i][j] == 0):
          imagine.blit(empty, ( j * 20, i * 20))

    drona = pygame.image.load(ASSET_DIR + "drona.png")
    imagine.blit(drona, (y * 20, x * 20))
    return imagine

class Drone():
  def __init__(self, map, x, y):
    self.__map = map
    self.__pos = (x, y)
    self.__seen = []
    self.__crumbs = []

  def pos(self):
    return self.__pos

  def __inBounds(self, pos):
    x, y = pos
    xMax, yMax = self.__map.surface.shape
    return x >= 0 and x < xMax and y >= 0 and y < yMax

  def __canMoveTo(self, pos):
    x, y = pos
    m = self.__map.surface

    return self.__inBounds(pos) and m[x][y] == 0 and pos not in self.__seen

  def __nearby(self):
    x, y = self.__pos
    ps = [(x - 1, y), (x, y - 1), (x + 1, y), (x, y + 1)]

    return list(filter(self.__canMoveTo, ps))

  def moveDFS(self):
    if self.__pos not in self.__seen:
      self.__seen.append(self.__pos)

    nearby = self.__nearby()

    if nearby:
      self.__crumbs.append(self.__pos)
      self.__pos = nearby.pop()
    elif self.__crumbs:
      self.__pos = self.__crumbs.pop()
