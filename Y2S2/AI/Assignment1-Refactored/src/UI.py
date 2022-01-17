import pygame
from random import randint

from Constants import *
from Env import *
from Domain import *
from Controller import *

class UI:
  def __init__(self, stepTime):
    self.__stepTime = stepTime

    e = Environment()
    e.loadEnvironment(DATA_DIR + "test2.map")

    m = Map()
    d = Drone(m, randint(0, 19), randint(0, 19))
    self.__ctl = Controller(e, m, d)

    pygame.init()

    logo = pygame.image.load(ASSET_DIR + "logo32x32.png")
    pygame.display.set_icon(logo)
    pygame.display.set_caption("drone exploration")

    self.__screen = pygame.display.set_mode((800,400))
    self.__screen.fill(WHITE)
    self.__screen.blit(e.image(), (0,0))

  def run(self):
    running = True

    timeLastFrame = 0
    timeSinceStep = 0

    while running:
      for event in pygame.event.get():
        if event.type == pygame.QUIT:
          running = False

      currentTime = pygame.time.get_ticks()
      deltaTime = currentTime - timeLastFrame
      timeLastFrame = currentTime
      timeSinceStep += deltaTime

      if timeSinceStep < self.__stepTime:
        continue

      timeSinceStep = 0
      self.__ctl.updateState()
      self.__screen.blit(self.__ctl.mapImage(), (400, 0))
      pygame.display.flip()

    pygame.quit()
