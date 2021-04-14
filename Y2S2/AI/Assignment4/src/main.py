#!/usr/bin/env python3

from map import Map
from gui import show_map

def main() -> None:
  show_map(Map.randomized())

if __name__ == "__main__":
  main()
