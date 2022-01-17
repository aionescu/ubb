from time import time
from typing import Any, Callable, Dict, List, TypeVar
import matplotlib.pyplot as plt # type: ignore

from map import Map
from domain import Individual
from controller import Controller
import gui

T = TypeVar("T")

def timed(fun: Callable[..., T]) -> Callable[..., T]:
  def timed_f(*args: List[Any], **kw: Dict[str, Any]) -> T:
    t0 = time()
    result = fun(*args, **kw)
    t1 = time()

    print(f"{fun.__name__} took {t1 - t0}s")
    return result

  return timed_f

class UI():
  def __init__(self, ctl: Controller) -> None:
    self.__ctl = ctl

    self.__cmds: Dict[str, Callable[[], None]] = {
      "random map": self.random_map,
      "load map": self.load_map,
      "save map": self.save_map,
      "show map": self.show_map,
      "run solver": self.run_solver,
      "run solver 30": self.run_solver_30,
      "show path": self.show_path,
      "show plot": self.show_plot,
      "show stats": self.show_stats,
      "help": self.help,
      "exit": quit
    }

  def random_map(self) -> None:
    self.__ctl.map = Map.randomized()

  def load_map(self) -> None:
    self.__ctl.map = Map.load("../data/map.pickle")

  def save_map(self) -> None:
    self.__ctl.map.save("../data/map.pickle")

  def show_map(self) -> None:
    gui.show_map(self.__ctl.map)

  @timed
  def run_solver(self) -> None:
    self.__ctl.solver()

  @timed
  def run_solver_30(self) -> None:
    for i in range(30):
      print(f"Performing run #{i + 1}")
      self.__ctl.solver()

  def show_plot(self) -> None:
    plt.plot(self.__ctl.avg_fitnesses_last_run)
    plt.xlabel("Avg fitness per generation")
    plt.show()

  def show_stats(self) -> None:
    print("Fitness of solution of each run:")
    print(f"Avg: {self.__ctl.solutions_fitness_avg}, Stddev: {self.__ctl.solutions_fitness_stddev}")

  def show_path(self) -> None:
    m = self.__ctl.map
    p = self.__ctl.initial_pos

    gui.draw_path(m, self.__ctl.last_solution.compute_path(m, p))

  def help(self) -> None:
    print("Available commands:")
    for cmd in self.__cmds:
      print(cmd)

  def run(self) -> None:
    while True:
      cmd = input("\n╭─[drone-exploration]\n╰─λ ")

      if cmd in self.__cmds:
        self.__cmds[cmd]()
      else:
        print("Command not recognized.")
