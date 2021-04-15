#!/usr/bin/env python3

from graph import Graph
from map import Map
from ant import Ant
from gui import draw_path, show_image_loop

def main() -> None:
  m = Map.randomized()
  g = Graph(m)

  best_ants = Ant.run_epochs(g, 100, 100, 100, 1, 1, 0.5)
  best = max(best_ants, key = lambda a: a.fitness)
  path = list(map(lambda m: m[0], best.path))

  show_image_loop(m.width, m.height, draw_path(m, g, path))

if __name__ == "__main__":
  main()
