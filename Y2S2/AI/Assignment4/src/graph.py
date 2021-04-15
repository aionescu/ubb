from typing import Dict, List, Tuple

from map import Map, Point

class Graph:
  def __init__(self, m: Map) -> None:
    self.__pos = m.sensors
    self.__area_per_energy = { sensor: m.visible_area_per_energy(p) for sensor, p in enumerate(self.__pos) }

    self.__dist: Dict[Tuple[int, int], int] = { }
    self.__path: Dict[Tuple[int, int], List[Point]] = { }

    sensor_count = len(self.__pos)

    for i in range(sensor_count):
      path = m.get_path(m.initial_pos, self.__pos[i])
      if path is None:
        raise ValueError("Invalid map: There are sensors that are unreacahble from the starting point.")

      self.__dist[(-1, i)] = len(path)
      self.__path[(-1, i)] = path

    for i in range(sensor_count - 1):
      for j in range(i, sensor_count):
        path = m.get_path(self.__pos[i], self.__pos[j])
        if path is None:
          raise ValueError("Invalid map: There are sensors with no path between them.")

        self.__dist[(i, j)] = len(path)
        self.__dist[(j, i)] = len(path)
        self.__path[(i, j)] = path
        self.__path[(j, i)] = path

    self.__trace: Dict[Tuple[int, int, int], float] = { }

  @property
  def sensor_count(self) -> int:
    return len(self.__pos)

  def area_per_energy(self, sensor: int) -> List[int]:
    return self.__area_per_energy[sensor]

  def dist(self, crr_sensor: int, target_sensor: int) -> int:
    return self.__dist[(crr_sensor, target_sensor)]

  def next_moves(self, crr_sensor: int) -> List[Tuple[int, int]]:
    moves = []

    for i in range(len(self.__pos)):
      if i == crr_sensor:
        continue

      for energy in range(len(self.__area_per_energy[i])):
        moves.append((i, energy))

    return moves

  def cost(self, crr_sensor: int, target_sensor: int, energy: int) -> int:
    return self.__dist[(crr_sensor, target_sensor)] + energy

  def quality(self, crr_sensor: int, target_sensor: int, energy: int) -> float:
    cost = self.cost(crr_sensor, target_sensor, energy)
    area = self.__area_per_energy[target_sensor][energy]

    return (1 + area) / cost

  def trace(self, crr_sensor: int, target_sensor: int, energy: int) -> float:
    return self.__trace.get((crr_sensor, target_sensor, energy)) or 1

  def evaporate_pheromones(self, evaporation_rate: float) -> None:
    for key in self.__trace:
      self.__trace[key] *= (1 - evaporation_rate)

  def add_pheromone(self, crr_sensor: int, target_sensor: int, energy: int, pheromone: float) -> None:
    self.__trace[(crr_sensor, target_sensor, energy)] = self.trace(crr_sensor, target_sensor, energy) + pheromone
