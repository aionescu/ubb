from random import randint
from exn import HitAsteroidError, HitEarthError, AlreadyHitError
from board import Board, MAP_SIZE, EARTH, ASTEROID, EMPTY, SHIP, HIT

class Game:
  def __init__(self):
    self.__board = Board()
    self.__ships = []
    self.__offsets = {}
    self.__cheat = False

    self.__place_earth()
    self.__place_asteroids()

    self.__place_ship()
    self.__place_ship()

  def __place_earth(self):
    self.__board.set((3, 3), EARTH)

  def __gen_asteroids(self):
    def gen():
      return (randint(0, MAP_SIZE - 1), randint(0, MAP_SIZE - 1))

    def is_too_close(a, b):
      ax, ay = a
      bx, by = b
      return abs(ax - bx) < 2 and abs(ay - by) < 2

    asteroids = []

    while len(asteroids) < 8:
      new = gen()

      while any(map(lambda b: is_too_close(new, b), asteroids)) or new == (3, 3):
        new = gen()

      asteroids.append(new)

    return asteroids

  def __place_asteroids(self):
    asteroids = self.__gen_asteroids()

    for asteroid in asteroids:
      self.__board.set(asteroid, ASTEROID)

  def __gen_ship(self, offset = 0):
    def gen_coord():
      return randint(0, MAP_SIZE - 1)

    edge = randint(0, 3)

    if edge == 0:
      return (0 + offset, gen_coord())
    elif edge == 1:
      return (gen_coord(), 0 + offset)
    elif edge == 2:
      return (MAP_SIZE - 1 - offset, gen_coord())
    else:
      return (gen_coord(), MAP_SIZE - 1 - offset)

  def __place_ship(self):
    ship = self.__gen_ship()

    while self.__board.get(ship) != EMPTY:
      ship = self.__gen_ship()

    self.__ships.append(ship)
    self.__offsets[ship] = 0
    self.__board.set(ship, SHIP)

  def cheat(self):
    self.__cheat = True

  def won(self):
    return not self.__ships

  def lost(self):
    def is_too_close(a, b):
      ax, ay = a
      bx, by = b
      return abs(ax - bx) < 2 and abs(ay - by) < 2

    return any(map(lambda b: is_too_close((3, 3), b), self.__ships))

  def advance_turn(self):
    self.__ships_advance()

  def __move_ship_to(self, ship, new_ship):
    self.__ships.remove(ship)
    offset = self.__offsets[ship]
    del self.__offsets[ship]
    
    self.__ships.append(new_ship)
    self.__offsets[new_ship] = offset

    self.__board.set(ship, EMPTY)
    self.__board.set(new_ship, SHIP)

  def __try_move_closer(self, ship):
    def closer_coord(coord):
      if coord < 3:
        return coord + 1
      elif coord > 3:
        return coord - 1
      else:
        return coord
    
    x, y = ship
    new_ship = (closer_coord(x), closer_coord(y))

    if self.__board.get(new_ship) != EMPTY:
      return False

    self.__move_ship_to(ship, new_ship)
    self.__offsets[new_ship] += 1

    return True
  
  def __teleport(self, ship):
    offset = self.__offsets[ship]

    new_ship = self.__gen_ship(offset)
    while self.__board.get(new_ship) != EMPTY:
      new_ship = self.__gen_ship(offset)
    
    self.__move_ship_to(ship, new_ship)

  def __advance_ship(self, ship):
    move_closer = randint(0, 1)

    if move_closer == 0:
      if not self.__try_move_closer(ship):
        self.__teleport(ship)
    else:
      self.__teleport(ship)

  def __ships_advance(self):

    for ship in self.__ships.copy():
      self.__advance_ship(ship)

  # Function that returns the positions of all asteroids on the map.
  # Input: -
  # Output: An iterable of tuples representing the coordinates of all asteroids.
  # Raises: -
  def all_asteroids(self):
    return self.all_eq(ASTEROID)

  # Function that returns the positions of all empty cells on the map.
  # Input: -
  # Output: An iterable of tuples representing the coordinates of all empty cells.
  # Raises: -
  def all_empty(self):
    return self.all_eq(EMPTY)

  # Function that returns the positions of all alien ships on the map.
  # Input: -
  # Output: An iterable of tuples representing the coordinates of all alien ships.
  # Raises: -
  def all_ships(self):
    return self.all_eq(SHIP)

  def all_eq(self, val):
    for x in range(MAP_SIZE):
      for y in range(MAP_SIZE):
        if self.__board.get((x, y)) == val:
          yield (x, y)

  # Function that attempts to hit the specified position
  # Input: pos - The position to hit
  # Output: True if a ship was hit, otherwise False
  # Raises:
  #   - HitEarthError: If the position to hit coincides with the Earth
  #   - HitAsteroidError: If there is an asteroid at the position to hit
  #   - AlreadyHitError: If the position to hit has already been hit
  def fire_exn(self, pos):
    cell = self.__board.get(pos)

    if cell == EARTH:
      raise HitEarthError()
    elif cell == ASTEROID:
      raise HitAsteroidError()
    elif cell == HIT:
      raise AlreadyHitError()

    if cell == SHIP:
      self.__ships.remove(pos)
      del self.__offsets[pos]

      self.__board.set(pos, HIT)
      return True
    
    if cell == EMPTY:
      self.__board.set(pos, HIT)
      return False

    raise Exception("Unreachable")

  # Function that parses a coordinate string into a coordinate
  # Input: s - The string to parse
  # Output: A tuple containing the x and y components of the coordinate,
  # if the string is a valid coordinate, otherwise None.
  # Raises: -
  def parse_coord(self, s):
    if len(s) != 2:
      return None

    y = ord(s[0]) - ord('A')

    if y < 0 or y >= MAP_SIZE:
      return None

    try:
      x = int(s[1])
    except ValueError:
      return None

    x -= 1
    if x < 0 or x >= MAP_SIZE:
      return None

    return (x, y)

  def __str__(self):
    return self.__board.to_str(self.__cheat)