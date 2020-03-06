from typing import Dict, List, Tuple, Callable

class Graph:
  def __init__(self):
    self.__vertexCount = 0
    self.__edgeCount = 0
    self.__inbound: Dict[int, List[int]] = {}
    self.__outbound: Dict[int, List[int]] = {}
    self.__cost: Dict[Tuple[Int, Int], Int] = {}

  def vertexCount(self) -> int:
    return self.__vertexCount

  def __assertInRange(self, vertex: int) -> None:
    if vertex < 0 or vertex >= self.__vertexCount:
      raise ValueError()

  def forEachVertex(self, vertex: int, action: Callable[[int], None]) -> None:
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
      raise ValueError()

    return self.__cost[pair]

  def setCost(self, vertex1: int, vertex2: int, cost: int) -> None:
    self.__assertInRange(vertex1)
    self.__assertInRange(vertex2)

    if not self.existsEdge(vertex1, vertex2):
      raise ValueError()

    self.__cost[(vertex1, vertex2)] = cost