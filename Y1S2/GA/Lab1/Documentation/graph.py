from copy import deepcopy
from random import randint
from typing import Dict, List, Tuple, Callable

# Class that represents a directed graph.
class Graph:
  # Representation of the graph:
  # * The graph is represented by 3 dictionaries,
  #   one that stores the inbound edges for each vertex, one that stores
  #   the outbound edges for each vertex, and one that stores the cost
  #   associated to each edge.
  # * The existence of a vertex can be checked by simply checking if it
  #   exists as a key in either the `__inbound` or `__outbound` dictionaries.
  # * The existence of an edge can be checked by simply checking if it exists
  #   as a key in the `__cost` dictionary.
  def __init__(self) -> None:
    self.__inbound: Dict[int, List[int]] = {}
    self.__outbound: Dict[int, List[int]] = {}
    self.__cost: Dict[Tuple[int, int], int] = {}

  # Returns the number of vertices in the graph.
  def vertexCount(self) -> int:
    return len(self.__outbound)

  # Checks whether the graph contains the specified vertex.
  # Returns: `True` if the vertex exists in the graph, otherwise `False`.
  def isVertex(self, vertex: int) -> bool:
    return vertex in self.__outbound

  def __assertVertexExists(self, vertex: int) -> None:
    if not self.isVertex(vertex):
      raise Exception("Vertex does not exist.")

  # Returns a list containing all vertices in the graph, in ascending order.
  def vertices(self) -> List[int]:
    return list(self.__outbound.keys())

  # Returns a list containing all the inbound edges of the specified vertex.
  # Raises: Exception if the specified vertex is not in the graph.
  # Law: forall v. forall v2 in inbound(v). existsEdge(v2, v)
  def inbound(self, vertex: int) -> List[int]:
    self.__assertVertexExists(vertex)

    return list(self.__inbound[vertex])

  # Returns a list containing all the outbound edges of the specified vertex.
  # Raises: Exception if the specified vertex is not in the graph.
  # Law: forall v. forall v2 in outbound(v). existsEdge(v, v2)
  def outbound(self, vertex: int) -> List[int]:
    self.__assertVertexExists(vertex)

    return list(self.__outbound[vertex])

  # Checks whether there exists an edge between the 2 vertices.
  # Returns: `True` if there exists an edge between the 2 vertices,
  # otherwise `False`.
  # Raises: Exception if either of the vertices is not in the graph.
  def existsEdge(self, vertex1: int, vertex2: int) -> bool:
    self.__assertVertexExists(vertex1)
    self.__assertVertexExists(vertex2)

    return vertex2 in self.__outbound[vertex1]

  # Returns the in degree of the specified vertex.
  # Raises: Exception if the specified vertex is not in the graph.
  def inDegree(self, vertex: int) -> int:
    self.__assertVertexExists(vertex)

    return len(self.__inbound[vertex])

  # Returns the out degree of the specified vertex.
  # Raises: Exception if the specified vertex is not in the graph.
  def outDegree(self, vertex: int) -> int:
    self.__assertVertexExists(vertex)

    return len(self.__outbound[vertex])

  # Returns the cost associated to the edge between `vertex1` and `vertex2`.
  # Raises:
  #   * Exception if either vertex is not in the graph.
  #   * Exception if there is no edge between vertex1 and vertex2.
  def getCost(self, vertex1: int, vertex2: int) -> int:
    self.__assertVertexExists(vertex1)
    self.__assertVertexExists(vertex2)

    pair = (vertex1, vertex2)

    if pair not in self.__cost:
      raise Exception("Edge does not exist.")

    return self.__cost[pair]

  # Sets the cost associated to the edge between `vertex1` and `vertex2` to
  # be equal to `cost`.
  # Raises:
  #   * Exception if either vertex is not in the graph.
  #   * Exception if there is no edge between vertex1 and vertex2.
  def setCost(self, vertex1: int, vertex2: int, cost: int) -> None:
    self.__assertVertexExists(vertex1)
    self.__assertVertexExists(vertex2)

    if not self.existsEdge(vertex1, vertex2):
      raise Exception("Edge does not exist.")

    self.__cost[(vertex1, vertex2)] = cost

  # Adds a new edge between `vertex1` and `vertex2`, with the cost equal
  # to `cost`.
  # Raises:
  #   * Exception if either vertex is not in the graph.
  #   * Exception if there already exists an edge between
  #     vertex1 and vertex2.
  def addEdge(self, vertex1: int, vertex2: int, cost: int) -> None:
    self.__assertVertexExists(vertex1)
    self.__assertVertexExists(vertex2)

    if self.existsEdge(vertex1, vertex2):
      raise Exception("Edge already exists.")

    self.__outbound[vertex1].append(vertex2)
    self.__inbound[vertex2].append(vertex1)
    self.__cost[(vertex1, vertex2)] = cost

  # Removes the edge between `vertex1` and `vertex2`.
  # Raises:
  #   * Exception if either vertex is not in the graph.
  #   * Exception if there is no edge between vertex1 and vertex2.
  def removeEdge(self, vertex1: int, vertex2: int) -> None:
    self.__assertVertexExists(vertex1)
    self.__assertVertexExists(vertex2)

    if not self.existsEdge(vertex1, vertex2):
      raise Exception("Edge does not exist.")

    self.__outbound[vertex1].remove(vertex2)
    self.__inbound[vertex2].remove(vertex1)
    del self.__cost[(vertex1, vertex2)]

  # Adds the specified vertex to the graph.
  # Raises: Exception if the vertex already exists.
  def addVertex(self, vertex: int) -> None:
    if self.isVertex(vertex):
      raise Exception("Vertex already exists.")

    self.__outbound[vertex] = []
    self.__inbound[vertex] = []

  # Removes the specified vertex from the graph.
  # Raises: Exception if the vertex is not in the graph.
  def removeVertex(self, vertex: int) -> None:
    self.__assertVertexExists(vertex)

    for v2 in self.__outbound[vertex]:
      self.__inbound[v2].remove(vertex)
      del self.__cost[(vertex, v2)]

    del self.__outbound[vertex]

    for v2 in self.__inbound[vertex]:
      self.__outbound[v2].remove(vertex)
      del self.__cost[(v2, vertex)]

    del self.__inbound[vertex]

  # Returns a string representation of the graph.
  # Law: forall g. fromString(toString(g)) == g
  def __str__(self) -> str:
    s = ""

    for v1 in self.vertices():
      outbound = self.outbound(v1)
      inbound = self.inbound(v1)

      if not outbound and not inbound:
        s += str(v1) + "\n"
      else:
        for v2 in self.outbound(v1):
          s += str(v1) + " " + str(v2) + " " + str(self.getCost(v1, v2)) + "\n"

    return s

  # Returns an independent copy of the graph.
  def copy(self) -> 'Graph':
    return deepcopy(self)

  # Constructs a graph from the given string.
  @staticmethod
  def fromString(s: str) -> 'Graph':
    g = Graph()
    lines = s.split("\n")

    for line in map(lambda x: x.split(" "), lines):
      if int(line[1]) == -1:
        g.addVertex(int(line[0]))
        continue

      v1 = int(line[0])
      v2 = int(line[1])
      c = int(line[2])

      if not g.isVertex(v1):
        g.addVertex(v1)

      if not g.isVertex(v2):
        g.addVertex(v2)

      g.addEdge(v1, v2, c)

    return g

  # Constructs a graph from the given string, which is expected
  # to be in the "old" format (that assumes the graph contains all
  # vertices from 0 to n - 1).
  @staticmethod
  def fromStringOld(s: str) -> 'Graph':
    lines = s.split("\n")
    fstLine = lines[0].split(" ")

    vertexCount = int(fstLine[0])
    edgeCount = int(fstLine[1])

    g = Graph()

    for v in range(vertexCount):
      g.addVertex(v)

    for i in range(edgeCount):
      edge = lines[i + 1].split(" ")
      g.addEdge(int(edge[0]), int(edge[1]), int(edge[2]))

    return g

  # Creates a graph with `vertexCount` vertices numbered from 0
  # to `vertexCount` - 1, and `edgeCount` randomly generated edges.
  @staticmethod
  def randomGraph(vertexCount: int, edgeCount: int) -> 'Graph':
    g = Graph()

    for v in range(vertexCount):
      g.addVertex(v)

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