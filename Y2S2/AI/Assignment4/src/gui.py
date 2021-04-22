from ant import Node
from graph import Graph
from typing import List, Tuple

from os import environ
environ["PYGAME_HIDE_SUPPORT_PROMPT"] = "1"

from pygame.surface import Surface
import pygame

from map import Map, Point

Color = Tuple[int, int, int]

gray: Color = (100, 100, 100)
light_gray: Color = (200, 200, 200)
white: Color = (255, 255, 255)
blue: Color = (0, 0, 255)
green: Color = (0, 255, 0)
red: Color = (255, 0, 0)

def init_pygame(width: int, height: int) -> Surface:
  pygame.init()

  logo = pygame.image.load("../assets/icon.png")
  pygame.display.set_icon(logo)
  pygame.display.set_caption("Drone Exploration with Evolutionary Algorithm")

  screen = pygame.display.set_mode((width * 20, height * 20))
  screen.fill(white)
  return screen

def draw_path(m: Map, g: Graph, path: List[Node]) -> Surface:
  image = Surface((20 * m.width, 20 * m.height))
  image.fill(white)

  wall_tile = Surface((20, 20))
  wall_tile.fill(blue)

  path_color_green = 255.0
  path_color_red = 0.0

  path_tile = Surface((20, 20))
  path_tile.fill(green)

  revealed_tile = Surface((20, 20))
  revealed_tile.fill(light_gray)

  for x, y in m.positions():
    if m[(x, y)]:
      image.blit(wall_tile, (y * 20, x * 20))

  drone_img = pygame.image.load("../assets/drone.png")
  sensor_img = pygame.image.load("../assets/sensor.png")

  path_color_delta = 255.0 / (len(path) - 1)
  prev_sensor = -1

  for s, energy in path:
    points = m.visible_tiles(m.sensors[s], energy)

    for x, y in points:
      image.blit(revealed_tile, (y * 20, x * 20))

  for s, _ in path:
    p = g.path(prev_sensor, s)
    prev_sensor = s

    path_tile.fill((int(path_color_red), int(path_color_green), 0))

    for x, y in p:
      image.blit(path_tile, (y * 20, x * 20))

    path_color_green -= path_color_delta
    path_color_red += path_color_delta

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
