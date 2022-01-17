# Assignment 4 - Books

from test import run_all_tests
from ui import Ui

def main():
  ui = Ui()

  while True:
    ui.handle_input()

if __name__ == "__main__":
  run_all_tests(True)
  main()