# Assignment 3 - Family expenses

from expense import rand_exps
from test import run_all_tests
from cmd import handle_input

def main():
  exps, undo, redo = rand_exps(), [], []

  while True:
    handle_input(exps, undo, redo, input("expenses> "))

if __name__ == "__main__":
  run_all_tests(True)
  main()