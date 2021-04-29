#!/usr/bin/env python3

import matplotlib.pyplot as plt # type: ignore

from data import load_data
from solver import gen_centroids, get_domain, solver

def main() -> None:
  inputs, outputs = load_data("../data/dataset.csv")
  centroids = gen_centroids(get_domain(inputs))

  solver(centroids, inputs, outputs)
  plt.show()

if __name__ == "__main__":
  main()
