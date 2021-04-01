#!/usr/bin/env python3

from map import Map
from controller import Controller
from ui import UI

def main() -> None:
  ui = UI(Controller(100))
  ui.run()

if __name__ == "__main__" :
  main()
