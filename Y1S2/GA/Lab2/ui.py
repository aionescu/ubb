from typing import List, Dict, Callable
from graph import Graph

class ExitError(Exception):
  pass

class UI:
  def __init__(self) -> None:
    self.__graph = Graph()
    self.__cmds: Dict[str, Callable[[List[str]], None]] = {
      "help": self.help,
      "exit": self.exit,
      "vertex-count": self.vertexCount,
      "vertices": self.vertices,
      "is-vertex": self.isVertex,
      "exists-edge": self.existsEdge,
      "degree": self.degree,
      "adjacent": self.adjacent,
      "add-edge": self.addEdge,
      "remove-edge": self.removeEdge,
      "add-vertex": self.addVertex,
      "remove-vertex": self.removeVertex,
      "load": self.load,
      "print": self.print,
      "connected-components": self.connectedComponents
    }

  def help(self, args: List[str]) -> None:
    print("Available commands:")

    for cmd in self.__cmds.keys():
      print(cmd)

  def exit(self, args: List[str]) -> None:
    raise ExitError()

  def vertexCount(self, args: List[str]) -> None:
    print(self.__graph.vertexCount())

  def vertices(self, args: List[str]) -> None:
    print(self.__graph.vertices())

  def isVertex(self, args: List[str]) -> None:
    print(self.__graph.isVertex(int(args[0])))

  def existsEdge(self, args: List[str]) -> None:
    print(self.__graph.existsEdge(int(args[0]), int(args[1])))

  def degree(self, args: List[str]) -> None:
    print(self.__graph.degree(int(args[0])))

  def adjacent(self, args: List[str]) -> None:
    print(self.__graph.adjacent(int(args[0])))

  def addEdge(self, args: List[str]) -> None:
    self.__graph.addEdge(int(args[0]), int(args[1]))

  def removeEdge(self, args: List[str]) -> None:
    self.__graph.removeEdge(int(args[0]), int(args[1]))

  def addVertex(self, args: List[str]) -> None:
    self.__graph.addVertex(int(args[0]))

  def removeVertex(self, args: List[str]) -> None:
    self.__graph.removeVertex(int(args[0]))

  def load(self, args: List[str]) -> None:
    with open(args[0], "r") as f:
      s = f.read()
      self.__graph = Graph.fromStringOld(s)

  def print(self, args: List[str]) -> None:
    print(self.__graph)

  def connectedComponents(self, args: List[str]) -> None:
    components = self.__graph.connectedComponents()

    for subgraph in components:
      print(subgraph)

    print("# of CCs:", len(components))

  def handleCommand(self) -> bool:
    s = input("> ").split()

    if not s or s[0] not in self.__cmds:
      print("Command not recognized.")
      return True

    try:
      self.__cmds[s[0]](s[1:])
    except ExitError:
      return False
    except Exception as e:
      print(e)

    return True

  def mainLoop(self) -> None:
    while self.handleCommand():
      pass