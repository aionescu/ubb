from sys import argv

from Search import search_algorithms
from UI import UI

def main() -> None:
  if len(argv) == 1 or argv[1] not in search_algorithms:
    algorithms = ", ".join(search_algorithms.keys())
    print(f"Please specify a search algorithm out of: {algorithms}.")
  else:
    ui = UI(search_algorithms[argv[1]])
    ui.run()

if __name__ == "__main__":
  main()
