from random import randint, seed
from typing import List, Tuple

from map import Map, Point
from domain import Individual, Population

class Controller():
  def __init__(self, iteration_count: int) -> None:
    self.__m = Map.randomized()
    self.__p = self.__m.random_empty_pos()
    self.__pop = Population([])

    self.__iter_count = iteration_count
    self.__avg_fitness: List[float] = []
    self.__solutions: List[Individual] = []

  @property
  def initial_pos(self) -> Point:
    return self.__p

  @property
  def map(self) -> Map:
    return self.__m

  @map.setter
  def map(self, m: Map) -> None:
    self.__m = m
    self.__p = m.random_empty_pos()

  @property
  def iteration_count(self) -> int:
    return self.__iter_count

  @property
  def population(self) -> Population:
    return self.__pop

  @property
  def avg_fitnesses_last_run(self) -> List[float]:
    return self.__avg_fitness

  @property
  def solutions_fitness_avg(self) -> float:
    return Population(self.__solutions).fitness_avg()

  @property
  def last_solution(self) -> Individual:
    return self.__solutions[-1]

  @property
  def solutions_fitness_stddev(self) -> float:
    return Population(self.__solutions).fitness_stddev()

  def one_iteration(self) -> None:
    pop_size = self.__pop.size

    self.__pop = self.__pop.crossover_all()
    self.__pop.evaluate_all(self.__m, self.__p)
    self.__pop = self.__pop.select_fittest(pop_size)

  def run(self) -> Tuple[List[float], Individual]:
    m = self.__m
    p = self.__p

    self.__pop.evaluate_all(m, p)

    solution = self.__pop.fittest()
    avgs = [self.__pop.fitness_avg()]

    for _ in range(self.__iter_count):
      self.one_iteration()
      candidate = self.__pop.fittest()

      if candidate.fitness > solution.fitness:
        solution = candidate

      avgs.append(self.__pop.fitness_avg())

    return avgs, solution

  def solver(self, individual_size: int = 20, population_size: int = 100) -> None:
    crr_seed = randint(1, 1000)
    seed(crr_seed)

    self.__pop = Population.randomized(individual_size, population_size)
    avgs, solution = self.run()

    self.__avg_fitness = avgs
    self.__solutions.append(solution)
