from enum import Enum
from random import *
from typing import List, Optional, Set, Tuple, Type
import numpy as np

from map import Dir, Map, Point, move_point, random_dir

Gene = Dir
Chromosome = List[Gene]

class Individual:
  def __init__(self, chromosome: Chromosome) -> None:
    self.__chromosome = chromosome
    self.__fitness = 0.0

  @staticmethod
  def randomized(size: int) -> 'Individual':
    return Individual([random_dir() for _ in range(size)])

  @property
  def fitness(self) -> float:
    return self.__fitness

  def compute_fitness(self, m: Map, p: Point) -> None:
    area: Set[Point] = set.union(*map(m.visible_area, self.compute_path(m, p)))
    self.__fitness = len(area)

  def compute_path(self, m: Map, p: Point) -> List[Point]:
    path = [p]

    for gene in self.__chromosome:
      p = move_point(p, gene)
      if not m.is_empty(p):
        break

      path.append(p)

    return path

  def mutate(self, probability: float = 0.04) -> 'Individual':
    if random() >= probability:
      return self
    else:
      genes = self.__chromosome.copy()

      gene_idxs = list(range(len(genes)))
      shuffle(gene_idxs)

      fst, snd = gene_idxs[0], gene_idxs[1]
      genes[fst], genes[snd] = genes[snd], genes[fst]

      return Individual(genes)

  def crossover(self, other: 'Individual', probability: float = 0.8) -> Optional[Tuple['Individual', 'Individual']]:
    if random() >= probability:
      return None
    else:
      cut = randint(1, len(self.__chromosome) - 1)

      offspring1 = Individual(self.__chromosome[:cut] + other.__chromosome[cut:])
      offspring2 = Individual(other.__chromosome[:cut] + self.__chromosome[cut:])

      return offspring1, offspring2

class Population():
  def __init__(self, individuals: List[Individual]) -> None:
    self.__individuals = individuals

  @staticmethod
  def randomized(individual_size: int, population_size: int) -> 'Population':
    return Population([Individual.randomized(individual_size) for _ in range(population_size)])

  @property
  def individuals(self) -> List[Individual]:
    return self.__individuals

  @property
  def size(self) -> int:
    return len(self.__individuals)

  def evaluate_all(self, m: Map, p: Point) -> None:
    for i in self.__individuals:
      i.compute_fitness(m, p)

  def select_parents(self) -> List[Tuple[Individual, Individual]]:
    return [(a, b) for a in self.__individuals for b in self.__individuals if a is not b]

  def select_fittest(self, k: int) -> 'Population':
    individuals = self.__individuals.copy()
    individuals.sort(key = lambda i: i.fitness, reverse = True)
    return Population(individuals[:k])

  def fittest(self) -> Individual:
    return self.select_fittest(1).__individuals[0]

  def crossover_all(self) -> 'Population':
    parents = self.__individuals.copy()
    shuffle(parents)

    new_pop = []

    for i in range(len(parents) // 2):
      p1, p2 = parents[i], parents[i * 2]
      offspring = p1.crossover(p2)

      if offspring is None:
        new_pop.append(p1.mutate())
        new_pop.append(p2.mutate())
      else:
        o1, o2 = offspring
        new_pop.append(o1)
        new_pop.append(o2)

    return Population(new_pop)

  def mutate_all(self) -> None:
    for i in self.__individuals:
      i.mutate()

  def fitness_avg(self) -> float:
    return np.average(list(map(lambda i: i.fitness, self.__individuals))) # type: ignore

  def fitness_stddev(self) -> float:
    return np.std(list(map(lambda i: i.fitness, self.__individuals))) # type: ignore
