from time import sleep
from typing import List, Tuple
from pygame.surface import Surface
import pygame

from map import Map, Point

WHITE = (255, 255, 255)
BLUE = (0, 0, 255)
GREEN = (0, 255, 0)

def init_pygame(dimension: Tuple[int, int]) -> Surface:
  pygame.init()

  logo = pygame.image.load("../assets/logo32x32.png")
  pygame.display.set_icon(logo)
  pygame.display.set_caption("Drone Exploration with Evolutionary Algorithm")

  screen = pygame.display.set_mode(dimension)
  screen.fill(WHITE)
  return screen

def pygame_main_loop() -> None:
  while pygame.QUIT not in map(lambda e: e.type, pygame.event.get()):
    pygame.display.flip()

  pygame.quit()

def map_image(m: Map) -> Surface:
  image = Surface((20 * m.width, 20 * m.height))
  brick = Surface((20, 20))

  image.fill(WHITE)
  brick.fill(BLUE)

  for (x, y) in m.positions():
    if m[(x, y)] == 1:
      image.blit(brick, (y * 20, x * 20))

  return image

def show_map(m: Map) -> None:
  screen = init_pygame((m.width * 20, m.height * 20))
  screen.blit(map_image(m), (0, 0))
  pygame_main_loop()

def draw_path(m: Map, path: List[Point], speed: float = 1, markSeen: bool = True) -> None:
  screen = init_pygame((m.width * 20, m.height * 20))
  drona = pygame.image.load("../assets/drona.png")

  brick = Surface((20,20))
  brick.fill(GREEN)

  screen.blit(map_image(m), (0, 0))
  pygame.display.flip()

  for p in path:
    x, y = p

    if markSeen:
      for vx, vy in m.visible_area(p):
        screen.blit(brick, (vy * 20, vx * 20))

    screen.blit(drona, (y * 20, x * 20))
    pygame.display.flip()

    sleep(0.5 * speed)

  pygame_main_loop()
