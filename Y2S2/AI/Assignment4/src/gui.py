from graph import Graph
from typing import List, Tuple
from pygame.surface import Surface
import pygame

from map import Map

Color = Tuple[int, int, int]

gray: Color = (100, 100, 100)
white: Color = (255, 255, 255)
blue: Color = (0, 0, 255)
green: Color = (0, 255, 0)

def init_pygame(width: int, height: int) -> Surface:
  pygame.init()

  logo = pygame.image.load("../assets/icon.png")
  pygame.display.set_icon(logo)
  pygame.display.set_caption("Drone Exploration with Evolutionary Algorithm")

  screen = pygame.display.set_mode((width * 20, height * 20))
  screen.fill(white)
  return screen

def draw_path(m: Map, g: Graph, path: List[int]) -> Surface:
  image = Surface((20 * m.width, 20 * m.height))
  image.fill(white)

  wall_tile = Surface((20, 20))
  wall_tile.fill(gray)

  path_tile = Surface((20, 20))
  path_tile.fill(green)

  for x, y in m.positions():
    if m[(x, y)]:
      image.blit(wall_tile, (y * 20, x * 20))

  drone_img = pygame.image.load("../assets/drone.png")
  sensor_img = pygame.image.load("../assets/sensor.png")

  prev_sensor = -1

  for s in path:
    p = g.path(prev_sensor, s)
    prev_sensor = s

    for x, y in p:
      image.blit(path_tile, (y * 20, x * 20))

  drone_x, drone_y = m.initial_pos
  image.blit(drone_img, (drone_y * 20, drone_x * 20))

  for sensor_x, sensor_y in m.sensors:
    image.blit(sensor_img, (sensor_y * 20, sensor_x * 20))

  return image

def pygame_main_loop() -> None:
  while pygame.QUIT not in map(lambda e: e.type, pygame.event.get()):
    pygame.display.flip()

  pygame.quit()

def show_image_loop(width: int, height: int, img: Surface) -> None:
  screen = init_pygame(width, height)
  screen.blit(img, (0, 0))
  pygame_main_loop()
