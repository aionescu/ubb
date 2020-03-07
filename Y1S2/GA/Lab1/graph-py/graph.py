from random import randint
from typing import Dict, List, Tuple, Callable

class Graph:
  def __init__(self, vertexCount: int = 0) -> None:
    self.__vertexCount = vertexCount
    self.__edgeCount = 0
    self.__inbound: Dict[int, List[int]] = {}
    self.__outbound: Dict[int, List[int]] = {}
    self.__cost: Dict[Tuple[int, int], int] = {}

  def vertexCount(self) -> int:
    return self.__vertexCount

  def __assertInRange(self, vertex: int) -> None:
    if vertex < 0 or vertex >= self.__vertexCount:
      raise Exception("Vertex out of range.")

  def forEachVertex(self, action: Callable[[int], None]) -> None:
    for i in range(self.__vertexCount):
      action(i)

  def existsEdge(self, vertex1: int, vertex2: int) -> bool:
    self.__assertInRange(vertex1)
    self.__assertInRange(vertex2)

    if vertex1 not in self.__outbound:
      return False

    for v in self.__outbound[vertex1]:
      if v == vertex2:
        return True

    return False

  def inDegree(self, vertex: int) -> int:
    self.__assertInRange(vertex)

    if vertex not in self.__inbound:
      return 0

    return len(self.__inbound[vertex])

  def outDegree(self, vertex: int) -> int:
    self.__assertInRange(vertex)

    if vertex not in self.__outbound:
      return 0

    return len(self.__outbound[vertex])

  def forEachInbound(self, vertex: int, action: Callable[[int], None]) -> None:
    self.__assertInRange(vertex)

    if vertex not in self.__inbound:
      return

    for v in self.__inbound[vertex]:
      action(v)

  def forEachOutbound(self, vertex: int, action: Callable[[int], None]) -> None:
    self.__assertInRange(vertex)

    if vertex not in self.__outbound:
      return

    for v in self.__outbound[vertex]:
      action(v)

  def getCost(self, vertex1: int, vertex2: int) -> int:
    self.__assertInRange(vertex1)
    self.__assertInRange(vertex2)

    pair = (vertex1, vertex2)

    if pair not in self.__cost:
      raise Exception("Edge does not exist.")

    return self.__cost[pair]

  def setCost(self, vertex1: int, vertex2: int, cost: int) -> None:
    self.__assertInRange(vertex1)
    self.__assertInRange(vertex2)

    if not self.existsEdge(vertex1, vertex2):
      raise Exception("Edge does not exist.")

    self.__cost[(vertex1, vertex2)] = cost

  def addEdge(self, vertex1: int, vertex2: int, cost: int) -> None:
    self.__assertInRange(vertex1)
    self.__assertInRange(vertex2)

    if self.existsEdge(vertex1, vertex2):
      raise Exception("Edge already exists.")

    if vertex1 not in self.__outbound:
      self.__outbound[vertex1] = []

    self.__outbound[vertex1].append(vertex2)

    if vertex2 not in self.__inbound:
      self.__inbound[vertex2] = []

    self.__inbound[vertex2].append(vertex1)

    self.__cost[(vertex1, vertex2)] = cost
    self.__edgeCount += 1

  def removeEdge(self, vertex1: int, vertex2: int) -> None:
    self.__assertInRange(vertex1)
    self.__assertInRange(vertex2)

    if not self.existsEdge(vertex1, vertex2):
      raise Exception("Edge does not exist.")

    self.__outbound[vertex1].remove(vertex2)
    self.__inbound[vertex2].remove(vertex1)

    del self.__cost[(vertex1, vertex2)]
    self.__edgeCount -= 1

  def addVertex(self) -> None:
    self.__vertexCount += 1

  def removeVertex(self, vertex: int) -> None:
    self.__assertInRange(vertex)

    for i in range(self.__vertexCount):
      if self.existsEdge(vertex, i):
        self.removeEdge(vertex, i)

      if self.existsEdge(i, vertex):
        self.removeEdge(i, vertex)

    for i in range(vertex + 1, self.__vertexCount):
      for j in range(0, self.__vertexCount):
        if self.existsEdge(i, j):
          cost = self.getCost(i, j)
          self.removeEdge(i, j)
          self.addEdge(i - 1, j, cost)

    for i in range(vertex + 1, self.__vertexCount):
      for j in range(0, self.__vertexCount):
        if self.existsEdge(j, i):
          cost = self.getCost(j, i)
          self.removeEdge(j, i)
          self.addEdge(j, i - 1, cost)

    self.__vertexCount -= 1

  def __str__(self) -> str:
    s = str(self.__vertexCount) + " " + str(self.__edgeCount) + "\n"

    def append(s2: str) -> None:
      nonlocal s
      s = s + s2

    self.forEachVertex(lambda v1:
      self.forEachOutbound(v1, lambda v2:
        append(str(v1) + " " + str(v2) + " " + str(self.getCost(v1, v2)) + "\n")))

    return s

  @staticmethod
  def fromString(s: str) -> 'Graph':
    lines = s.split("\n")
    fstLine = lines[0].split(" ")

    vertexCount = int(fstLine[0])
    edgeCount = int(fstLine[1])

    g = Graph(vertexCount)

    for i in range(edgeCount):
      edge = lines[i + 1].split(" ")
      g.addEdge(int(edge[0]), int(edge[1]), int(edge[2]))

    return g

  @staticmethod
  def randomGraph(vertexCount: int, edgeCount: int) -> 'Graph':
    g = Graph(vertexCount)
    i = 0

    while i < edgeCount:
      v1 = randint(0, vertexCount - 1)
      v2 = randint(0, vertexCount - 1)
      cost = randint(-100, 100)

      try:
        g.addEdge(v1, v2, cost)
        i += 1
      except:
        pass

    return g