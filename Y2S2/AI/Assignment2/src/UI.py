import pygame
from typing import List, Tuple
from time import time
from pygame import Surface

from Map import Map, Point
from Search import Path, Search
from Controller import Controller

Color = Tuple[int, int, int]
WHITE: Color = (255, 255, 255)
BLUE: Color = (0, 0, 255)
GREEN: Color = (0, 255, 0)

class UI():
  def __init__(self, search: Search) -> None:
    self.__search = search

    m = Map()
    m.fill_random()

    self.__ctl = Controller(m)

    pygame.init()
    logo = pygame.image.load("assets/logo32x32.png")
    pygame.display.set_icon(logo)
    pygame.display.set_caption("Pathfinding in a Simple Environment")

    self.__screen = pygame.display.set_mode((20 * m.width, 20 * m.height))
    self.__screen.fill(WHITE)

  def map_image(self, empty_color: Color = WHITE, wall_color: Color = BLUE) -> Surface:
    image = Surface((20 * self.__ctl.map.width, 20 * self.__ctl.map.height))
    brick = Surface((20, 20))

    image.fill(empty_color)
    brick.fill(wall_color)

    for (x, y) in self.__ctl.map.positions():
      if self.__ctl.map[(x, y)] == 1:
        image.blit(brick, (y * 20, x * 20))

    return image

  def draw_path(self, path: List[Point], img: Surface):
    color_step = 255 / len(path)
    color_red = 0.0
    color_green = 255.0

    mark = Surface((20, 20))

    for (x, y) in path:
      mark.fill((int(color_red), int(color_green), 0))
      img.blit(mark, (y * 20, x * 20))

      color_red += color_step
      color_green -= color_step

    return img

  def run_search_timed(self) -> Tuple[float, Path]:
    start = time()
    result = self.__ctl.run_search(self.__search)
    end = time()

    return (end - start, result)

  def run(self):
    map_img = self.map_image()

    self.__screen.blit(map_img, (0, 0))
    pygame.display.flip()

    (time, path) = self.run_search_timed()
    print(f"Search took {time} seconds.")

    if path is None:
      print("No path found.")
    else:
      self.__screen.blit(self.draw_path(path, map_img), (0, 0))
      pygame.display.flip()

    while pygame.QUIT not in map(lambda e: e.type, pygame.event.get()):
      pygame.display.flip()

    pygame.quit()
