from typing import List, Dict, Callable
from graph import Graph

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
      "in-degree": self.inDegree,
      "out-degree": self.outDegree,
      "inbound": self.inbound,
      "outbound": self.outbound,
      "get-cost": self.getCost,
      "set-cost": self.setCost,
      "add-edge": self.addEdge,
      "remove-edge": self.removeEdge,
      "add-vertex": self.addVertex,
      "remove-vertex": self.removeVertex,
      "save-to-file": self.saveToFile,
      "load-from-file": self.loadFromFile,
      "load-old-fmt": self.loadOldFmt,
      "random": self.random,
      "print": self.print
    }

  def help(self, args: List[str]) -> None:
    print("Available commands:")

    for cmd in self.__cmds.keys():
      print(cmd)

  def exit(self, args: List[str]) -> None:
    exit()

  def vertexCount(self, args: List[str]) -> None:
    print(self.__graph.vertexCount())

  def vertices(self, args: List[str]) -> None:
    print(self.__graph.vertices())

  def isVertex(self, args: List[str]) -> None:
    print(self.__graph.isVertex(int(args[0])))

  def existsEdge(self, args: List[str]) -> None:
    print(self.__graph.existsEdge(int(args[0]), int(args[1])))

  def inDegree(self, args: List[str]) -> None:
    print(self.__graph.inDegree(int(args[0])))

  def outDegree(self, args: List[str]) -> None:
    print(self.__graph.outDegree(int(args[0])))

  def inbound(self, args: List[str]) -> None:
    print(self.__graph.inbound(int(args[0])))

  def outbound(self, args: List[str]) -> None:
    print(self.__graph.outbound(int(args[0])))

  def getCost(self, args: List[str]) -> None:
    print(self.__graph.getCost(int(args[0]), int(args[1])))

  def setCost(self, args: List[str]) -> None:
    self.__graph.setCost(int(args[0]), int(args[1]), int(args[2]))

  def addEdge(self, args: List[str]) -> None:
    self.__graph.addEdge(int(args[0]), int(args[1]), int(args[2]))

  def removeEdge(self, args: List[str]) -> None:
    self.__graph.removeEdge(int(args[0]), int(args[1]))

  def addVertex(self, args: List[str]) -> None:
    self.__graph.addVertex(int(args[0]))

  def removeVertex(self, args: List[str]) -> None:
    self.__graph.removeVertex(int(args[0]))

  def saveToFile(self, args: List[str]) -> None:
    with open(args[0], "w") as f:
      s = str(self.__graph)
      f.write(s)

  def loadFromFile(self, args: List[str]) -> None:
    with open(args[0], "r") as f:
      s = f.read()
      self.__graph = Graph.fromString(s)

  def loadOldFmt(self, args: List[str]) -> None:
    with open(args[0], "r") as f:
      s = f.read()
      self.__graph = Graph.fromStringOld(s)

  def print(self, args: List[str]) -> None:
    print(self.__graph)

  def random(self, args: List[str]) -> None:
    self.__graph = Graph.randomGraph(int(args[0]), int(args[1]))


  def handleCommand(self) -> None:
    s = input("> ").split()

    if not s or s[0] not in self.__cmds:
      print("Command not recognized.")
      return

    try:
      self.__cmds[s[0]](s[1:])
    except Exception as e:
      print(e)