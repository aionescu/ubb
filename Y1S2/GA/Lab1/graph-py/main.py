from graph import Graph

def readToString(fileName: str) -> str:
  with open(fileName, "r") as f:
    return f.read()

def main() -> None:
  s = readToString("../Input/digraph-ex1.txt")
  g = Graph.fromString(s)

  print(g)
  print(Graph.randomGraph(5, 10))

if __name__ == "__main__":
  main()