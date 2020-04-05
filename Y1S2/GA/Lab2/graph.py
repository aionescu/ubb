from copy import deepcopy
from random import randint
from typing import Dict, List, Tuple, Callable

class Graph:
  def __init__(self) -> None:
    self.__adjacent: Dict[int, List[int]] = {}

  def vertexCount(self) -> int:
    return len(self.__adjacent)

  def isVertex(self, vertex: int) -> bool:
    return vertex in self.__adjacent

  def __assertVertexExists(self, vertex: int) -> None:
    if not self.isVertex(vertex):
      raise Exception("Vertex does not exist.")

  def vertices(self) -> List[int]:
    return list(self.__adjacent.keys())

  def adjacent(self, vertex: int) -> List[int]:
    self.__assertVertexExists(vertex)

    return list(self.__adjacent[vertex])

  def existsEdge(self, vertex1: int, vertex2: int) -> bool:
    self.__assertVertexExists(vertex1)
    self.__assertVertexExists(vertex2)

    return vertex2 in self.__adjacent[vertex1]

  def degree(self, vertex: int) -> int:
    self.__assertVertexExists(vertex)

    return len(self.__adjacent[vertex])

  def addEdge(self, vertex1: int, vertex2: int) -> None:
    self.__assertVertexExists(vertex1)
    self.__assertVertexExists(vertex2)

    if self.existsEdge(vertex1, vertex2):
      raise Exception("Edge already exists.")

    self.__adjacent[vertex1].append(vertex2)
    self.__adjacent[vertex2].append(vertex1)

  def removeEdge(self, vertex1: int, vertex2: int) -> None:
    self.__assertVertexExists(vertex1)
    self.__assertVertexExists(vertex2)

    if not self.existsEdge(vertex1, vertex2):
      raise Exception("Edge does not exist.")

    self.__adjacent[vertex1].remove(vertex2)
    self.__adjacent[vertex2].remove(vertex1)

  def addVertex(self, vertex: int) -> None:
    if self.isVertex(vertex):
      raise Exception("Vertex already exists.")

    self.__adjacent[vertex] = []

  def removeVertex(self, vertex: int) -> None:
    self.__assertVertexExists(vertex)

    for v2 in self.__adjacent[vertex]:
      self.__adjacent[v2].remove(vertex)

    del self.__adjacent[vertex]

  def dfs(self, temp: List[int], v: int, visited: List[bool]) -> List[int]:
    visited[v] = True
    temp.append(v) 

    for i in self.__adjacent[v]: 
      if not visited[i]:  
        temp = self.dfs(temp, i, visited)

    return temp 

  def connectedComponents(self) -> List[List[int]]: 
    visited = [False for _ in range(self.vertexCount())]
    cc = []

    for v in range(self.vertexCount()): 
      if self.isVertex(v) and not visited[v]:
        cc.append(self.dfs([], v, visited)) 

    return cc 

  def __str__(self) -> str:
    s = ""

    for v1 in self.vertices():
      if not self.degree(v1):
        s += str(v1) + " -1\n"
      else:
        for v2 in self.adjacent(v1):
          s += str(v1) + " " + str(v2) + "\n"

    return s

  @staticmethod
  def fromStringOld(s: str) -> 'Graph':
    lines = s.split("\n")
    fstLine = lines[0].split(" ")

    vertexCount = int(fstLine[0])
    edgeCount = int(fstLine[1])

    if edgeCount > vertexCount ** 2:
      raise Exception("Edge count exceeds vertex count ^ 2.")

    g = Graph()

    for v in range(vertexCount):
      g.addVertex(v)

    for i in range(edgeCount):
      edge = lines[i + 1].split(" ")

      try:
        g.addEdge(int(edge[0]), int(edge[1]))
      except:
        pass

    return g