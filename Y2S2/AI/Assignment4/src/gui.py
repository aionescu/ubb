from typing import Tuple
from pygame.surface import Surface
import pygame

from map import Map

Color = Tuple[int, int, int]

gray: Color = (100, 100, 100)
white: Color = (255, 255, 255)
blue: Color = (0, 0, 255)
green: Color = (0, 255, 0)

def init_pygame(dimension: Tuple[int, int]) -> Surface:
  pygame.init()

  logo = pygame.image.load("../assets/icon.png")
  pygame.display.set_icon(logo)
  pygame.display.set_caption("Drone Exploration with Evolutionary Algorithm")

  screen = pygame.display.set_mode(dimension)
  screen.fill(white)
  return screen

def map_image(m: Map) -> Surface:
  image = Surface((20 * m.width, 20 * m.height))
  brick = Surface((20, 20))

  image.fill(white)
  brick.fill(gray)

  for (x, y) in m.positions():
    if m[(x, y)]:
      image.blit(brick, (y * 20, x * 20))

  drone = pygame.image.load("../assets/drone.png")
  sensor = pygame.image.load("../assets/sensor.png")

  drone_x, drone_y = m.initial_pos
  image.blit(drone, (drone_y * 20, drone_x * 20))

  for sensor_x, sensor_y in m.sensors:
    image.blit(sensor, (sensor_y * 20, sensor_x * 20))

  return image

def show_map(m: Map) -> None:
  screen = init_pygame((m.width * 20, m.height * 20))
  screen.blit(map_image(m), (0, 0))
  pygame_main_loop()

def pygame_main_loop() -> None:
  while pygame.QUIT not in map(lambda e: e.type, pygame.event.get()):
    pygame.display.flip()

  pygame.quit()
