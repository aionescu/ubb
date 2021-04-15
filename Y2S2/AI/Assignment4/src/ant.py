from random import randint, random
from typing import List, Optional, Tuple, TypeVar

from graph import Graph

T = TypeVar("T")

def roulette_wheel(options: List[Tuple[T, float]]) -> T:
  options.sort(key = lambda o: o[1])
  cumulative = [0.0 for _ in options]

  cumulative[0] = options[0][1]

  for i in range(1, len(cumulative)):
    cumulative[i] = cumulative[i - 1] + options[i][1]

  r = random()
  for i, v in enumerate(cumulative):
    if v >= r:
      return options[i][0]

  raise ValueError("roulette_wheel: Unreachable.")
  # return options[-1][0]

Move = Tuple[int, int]

class Ant:
  def __init__(self, g: Graph, battery: int) -> None:
    sensor = randint(0, g.sensor_count - 1)
    energy = randint(0, len(g.area_per_energy(sensor)) - 1)
    self.__path = [(sensor, energy)]
    self.__fitness = g.quality(-1, sensor, energy)
    self.__battery = battery - g.cost(-1, sensor, energy)

  @property
  def path(self) -> List[Move]:
    return self.__path

  @property
  def fitness(self) -> float:
    return self.__fitness

  @property
  def battery(self) -> int:
    return self.__battery

  def __seen_sensor(self, sensor: int) -> bool:
    for s, _ in self.__path:
      if sensor == s:
        return True

    return False

  def __valid_move(self, g: Graph, crr_sensor: int, target_sensor: int, energy: int) -> bool:
    return not self.__seen_sensor(target_sensor) and g.cost(crr_sensor, target_sensor, energy) <= self.__battery

  def next_moves(self, g: Graph) -> List[Move]:
    crr_sensor, _ = self.__path[-1]
    return list(filter(lambda m: self.__valid_move(g, crr_sensor, *m), g.next_moves(crr_sensor)))

  def next_move(self, g: Graph, alpha: float, beta: float) -> Optional[Move]:
    crr_sensor, _ = self.__path[-1]
    next_moves = self.next_moves(g)

    if not next_moves:
      return None

    def compute_quality(move: Move) -> float:
      target_sensor, energy = move
      t = g.trace(crr_sensor, target_sensor, energy)
      q = g.quality(crr_sensor, target_sensor, energy)
      return (t ** alpha) * (q ** beta)

    next_move_qualities = list(map(compute_quality, next_moves))
    qualities_sum = sum(next_move_qualities)

    def compute_probability(i_move: Tuple[int, Move]) -> Tuple[Move, float]:
      i, move = i_move
      return (move, next_move_qualities[i] / qualities_sum)

    probabilites = list(map(compute_probability, enumerate(next_moves)))
    return roulette_wheel(probabilites)

  def make_move(self, g: Graph, move: Move) -> None:
    crr_sensor, _ = self.__path[-1]
    target_sensor, energy = move

    self.__battery -= g.cost(crr_sensor, target_sensor, energy)
    self.__fitness += g.quality(crr_sensor, target_sensor, energy)
    self.__path.append(move)

  @staticmethod
  def epoch(g: Graph, battery: int, ant_count: int, alpha: float, beta: float, evaporation_rate: float) -> 'Ant':
    ants = [Ant(g, battery) for _ in range(ant_count)]

    for _ in range(g.sensor_count - 1):
      for ant in ants:
        next_move = ant.next_move(g, alpha, beta)
        if next_move is not None:
          ant.make_move(g, next_move)

    pheromone = [ant.fitness for ant in ants]

    g.evaporate_pheromones(evaporation_rate)

    for ant_i, ant in enumerate(ants):
      for i in range(1, len(ant.__path)):
        x, _ = ant.__path[i - 1]
        y, e = ant.__path[i]
        g.add_pheromone(x, y, e, pheromone[ant_i])

    return max(ants, key = lambda ant: ant.fitness)

  @staticmethod
  def run_epochs(g: Graph, battery: int, epoch_count: int, ant_count: int, alpha: float, beta: float, evaporation_rate: float) -> List['Ant']:
    return list(map(lambda _: Ant.epoch(g, battery, ant_count, alpha, beta, evaporation_rate), range(epoch_count)))
