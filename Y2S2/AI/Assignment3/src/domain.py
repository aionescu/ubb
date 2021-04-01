from random import randint, random, shuffle
from typing import Dict, List, Optional, Set, Tuple, Type
import numpy as np

from map import Dir, Map, Point, manhattan, move_point, random_dir

Gene = Dir
Chromosome = List[Gene]

class FitnessComputeCache:
  def __init__(self, m: Map, p: Point) -> None:
    self.__m = m
    self.__p = p
    self.__horizontal: Dict[Point, Set[Point]] = {}
    self.__vertical: Dict[Point, Set[Point]] = {}

  @property
  def map(self) -> Map:
    return self.__m

  @property
  def initial_pos(self) -> Point:
    return self.__p

  def horizontal_area(self, p: Point) -> Set[Point]:
    if p in self.__horizontal:
      return self.__horizontal[p]
    else:
      area = self.__m.visible_area_horizontal(p)
      self.__horizontal[p] = area
      return area

  def vertical_area(self, p: Point) -> Set[Point]:
    if p in self.__vertical:
      return self.__vertical[p]
    else:
      area = self.__m.visible_area_vertical(p)
      self.__vertical[p] = area
      return area

  def visible_area(self, p: Point) -> Set[Point]:
    return self.horizontal_area(p).union(self.vertical_area(p))

class Individual:
  def __init__(self, chromosome: Chromosome, fcc: FitnessComputeCache) -> None:
    self.__chromosome = chromosome
    self.__fitness = self.compute_fitness(fcc)

  @staticmethod
  def randomized(size: int, fcc: FitnessComputeCache) -> 'Individual':
    return Individual([random_dir() for _ in range(size)], fcc)

  @property
  def fitness(self) -> int:
    return self.__fitness

  def compute_fitness(self, fcc: FitnessComputeCache) -> int:
    m = fcc.map
    p = fcc.initial_pos

    area = fcc.visible_area(p)

    for gene in self.__chromosome:
      p = move_point(p, gene)

      if not m.is_empty(p):
        break

      if gene is Dir.UP or gene is Dir.DOWN:
        area.update(fcc.horizontal_area(p))
      else:
        area.update(fcc.vertical_area(p))

    return len(area) // (manhattan(fcc.initial_pos, p) + 1)

  def compute_path(self, m: Map, p: Point) -> List[Point]:
    path = [p]

    for gene in self.__chromosome:
      p = move_point(p, gene)
      if not m.is_empty(p):
        break

      path.append(p)

    return path

  def mutate(self, fcc: FitnessComputeCache, probability: float = 0.04) -> 'Individual':
    if random() >= probability:
      return self
    else:
      genes = self.__chromosome.copy()

      gene_idxs = list(range(len(genes)))
      shuffle(gene_idxs)

      fst, snd = gene_idxs[0], gene_idxs[1]
      genes[fst], genes[snd] = genes[snd], genes[fst]

      return Individual(genes, fcc)

  def crossover(self, other: 'Individual', fcc: FitnessComputeCache, probability: float = 0.8) -> Optional[Tuple['Individual', 'Individual']]:
    if random() >= probability:
      return None
    else:
      cut = randint(1, len(self.__chromosome) - 1)

      offspring1 = Individual(self.__chromosome[:cut] + other.__chromosome[cut:], fcc)
      offspring2 = Individual(other.__chromosome[:cut] + self.__chromosome[cut:], fcc)

      return offspring1, offspring2

class Population:
  def __init__(self, individuals: List[Individual]) -> None:
    self.__individuals = individuals

  @staticmethod
  def randomized(individual_size: int, population_size: int, fcc: FitnessComputeCache) -> 'Population':
    return Population([Individual.randomized(individual_size, fcc) for _ in range(population_size)])

  @property
  def individuals(self) -> List[Individual]:
    return self.__individuals

  @property
  def size(self) -> int:
    return len(self.__individuals)

  def select_parents(self) -> List[Tuple[Individual, Individual]]:
    return [(a, b) for a in self.__individuals for b in self.__individuals if a is not b]

  def select_fittest(self, k: int) -> 'Population':
    individuals = self.__individuals.copy()
    individuals.sort(key = lambda i: i.fitness, reverse = True)
    return Population(individuals[:k])

  def fittest(self) -> Individual:
    return self.select_fittest(1).__individuals[0]

  def crossover_all(self, fcc: FitnessComputeCache) -> 'Population':
    parents = self.__individuals.copy()
    shuffle(parents)

    new_pop = list(map(lambda i: i.mutate(fcc), self.__individuals))

    for i in range(len(parents) // 2):
      p1, p2 = parents[i], parents[i * 2]
      offspring = p1.crossover(p2, fcc)

      if offspring is not None:
        o1, o2 = offspring
        new_pop.append(o1)
        new_pop.append(o2)

    return Population(new_pop)

  def fitness_avg(self) -> float:
    return np.average(list(map(lambda i: i.fitness, self.__individuals))) # type: ignore

  def fitness_stddev(self) -> float:
    return np.std(list(map(lambda i: i.fitness, self.__individuals))) # type: ignore
