import copy
import random
from random import randint

class DirectedGraph:
  def __init__(self, nrVertices, nrEdges):
    self.__nrVertices = nrVertices
    self.__nrEdges = nrEdges

    self._dictOut = {}
    self._dictIn = {}
    self._dictCosts = {}

    for i in range(nrVertices):
      self._dictOut[i] = []
      self._dictIn[i] = []

  def vertices(self):
    return self._dictOut.keys()

  def edges(self):
    return self._dictCosts

  def inbound(self, vertex):
    return self._dictIn[vertex]

  def outbound(self, vertex):
    return self._dictOut[vertex]

  def isEdge(self, outVertex, inVertex):
    if not self.isVertex(outVertex) or not self.isVertex(inVertex):
      raise Exception("Both vertices must exist.")

    return inVertex in self._dictOut[outVertex]
    
  def isVertex(self, vertex):
    return vertex in self._dictOut.keys()

  def addEdge(self, outVertex, inVertex, cost):
    if self.isEdge(outVertex, inVertex):
      raise Exception("Edge already exists.") 
    self._dictOut[outVertex].append(inVertex)
    self._dictIn[inVertex].append(outVertex)
    self._dictCosts[(outVertex, inVertex)] = cost

  def addVertex(self, vertex):
    if vertex in self.vertices():
      raise Exception("Vertex already exists.")

    self.__nrVertices += 1
    self._dictIn[vertex] = []
    self._dictOut[vertex] = []

  def getGraphFromFile(self, fileName):
    with open(fileName, "r") as f:
      f.readline()
      for i in range(self.__nrEdges):
        line = f.readline()
        line = line.split()
        print(line)
        outVertex = int(line[0])
        inVertex = int(line[1])
        cost = int(line[2])
        self.addEdge(outVertex, inVertex, cost)

  def writeGraphToFile(self, fileName):
    with open(fileName, "w") as f:
      line = ''
      line = line + str(self.getNrVertices()) + " " + str(self.getNrEdges()) + '\n'
      f.write(line)
      for edge in self._dictCosts:
        line = ''
        line = line + str(edge[0]) + ' ' + str(edge[1]) + ' ' + str(self.getCost(edge)) + '\n'
        f.write(line)

  def getNrVertices(self):
    self.__nrVertices = len(self._dictIn)
    return self.__nrVertices

  def getInDegree(self, vertex):
    if self.isVertex(vertex):
      return len(self._dictIn[vertex])
    print("There is no such vertex!")

  def getOutDegree(self, vertex):
    if self.isVertex(vertex):
      return len(self._dictOut[vertex])
    print("There is no such vertex!")

  def parseOutboundEdges(self, vertex):
    outBoundEdges = []
    for edge in self._dictCosts.keys():
      if edge[0] == vertex:
        outBoundEdges.append(edge)
    return outBoundEdges

  def parseInboundEdges(self, vertex):
    outInEdges = []
    for edge in self._dictCosts.keys():
      if edge[1] == vertex:
        outInEdges.append(edge)
    return outInEdges

  def getNrEdges(self):
    return len(self._dictCosts.keys())

  def removeEdge(self, edge_id):
    if not self.isEdge(edge_id[0], edge_id[1]):
      print("There is no such an edge!")
      return
    inVertex = edge_id[1]
    outVertex = edge_id[0]
    self._dictOut[outVertex].remove(inVertex)
    self._dictIn[inVertex].remove(outVertex)
    del self._dictCosts[edge_id]

  def removeVertex(self, vertex_id):
    if not self.isVertex(vertex_id):
      raise Exception("Vertex does not exist.")

    for inVertex in self._dictOut[vertex_id]:
      del self._dictCosts[(vertex_id, inVertex)]
      self._dictIn[inVertex].remove(vertex_id)

    del self._dictOut[vertex_id]

    for outVertex in self._dictIn[vertex_id]:
      del self._dictCosts[(outVertex, vertex_id)]
      self._dictOut[outVertex].remove(vertex_id)

    del self._dictIn[vertex_id]

  def getCost(self, edge_id):
    return self._dictCosts[edge_id]

  def modifyCost(self, edge_id, newCost):
    self._dictCosts[edge_id] = newCost

  def copyGraph(self):
    copyOfGraph = copy.deepcopy(self);
    return copyOfGraph

  def randomGraph(self, nrVertices, nrEdges):
    randomGraph = DirectedGraph(nrVertices,nrEdges)
    vertices = []

    for vertex in range(nrVertices):
      vertices.append(int(vertex))

    for i in range(nrEdges):
      outVertex = random.choice(vertices)
      inVertex = random.choice(vertices)

      while (outVertex, inVertex) in randomGraph._dictCosts.keys():
        outVertex = random.choice(vertices)
        inVertex = random.choice(vertices)

      randomGraph.addEdge(outVertex, inVertex, randint(-1000,1000))

    return randomGraph

  def printGraph(self):
    vertices = self.vertices()

    for vertex in vertices:
      print(vertex)

      print(self.inbound(vertex))
      print(self.outbound(vertex))

      print("\n")

    edges = self.edges()
    for edge in edges:
      print(str(edge) + " - " + str(edges[edge]))

  def TopologicalSort(self):
    sorted = []
    queue = []
    count = {}

    for x in self.vertices():
      count[x] = len(self.inbound(x))

      if count[x] == 0:
        queue.append(x)

    while not len(queue) == 0:
      x = queue[0]
      queue.remove(x)
      sorted.append(x)

      for y in self.outbound(x):
        count[y] = count[y] - 1
        if count[y] == 0:
          queue.append(y)

    if len(sorted) < self.__nrVertices:
      raise Exception("not a DAG!")

    print("A topological order is: ", sorted)
    return sorted

  def Schedule(self):
    sorted = self.TopologicalSort()

    timing = {}
    timing[0] = [0,0]

    sorted.remove(0)

    for x in sorted:
      timing[x] = []
      maxTime = 0

      for y in self.inbound(x):
        somePreviousNode = y

        if timing[y][1] > maxTime:
          maxTime = timing[y][1]

      duration = self.getCost((somePreviousNode, x))

      timing[x].append(maxTime)
      timing[x].append(maxTime+duration)

    print("early scheduling:",timing)

    sorted.reverse()

    Ltiming = {}
    Ltiming[sorted[0]] = [0, 0]

    sorted.remove(sorted[0])
    sorted.append(0)

    for x in sorted:
      Ltiming[x] = []
      minTime = 0

      for y in self.outbound(x):
        if Ltiming[y][0] < minTime:
          minTime = Ltiming[y][0]

      if x == 0:
        duration = 0
      else:
        duration = -self.getCost((self.inbound(x)[0], x))
        
      Ltiming[x].append(minTime+duration)
      Ltiming[x].append(minTime)

    print("\n")
    totalTime = -Ltiming[0][0]
    criticalPoints = []

    for x in Ltiming:
      Ltiming[x][0] = Ltiming[x][0]+totalTime
      Ltiming[x][1] = Ltiming[x][1]+totalTime

      if Ltiming[x][0] == timing[x][0] and Ltiming[x][1] == timing[x][1]:
        criticalPoints.append(x)

    print("later scheduling:", Ltiming, "\n")
    print("Critical Activities are: ", criticalPoints)


  def Activities(self, fileName):
    with open(fileName, "r") as f:
      f.readline()
      lines = f.readlines()

      for line in lines:
        line = line.split()
        outvertices = line[2].split(",")
        inVertex = int(line[0])

        for x in outvertices:
          outVertex = int(x)
          cost = int(line[1])
          self.addEdge(outVertex, inVertex, cost)

    self.printGraph()

  def nrPaths(self, source, dest):
    s = self.TopologicalSort()
    paths = []

    for vertex in s:
      paths.append(0)

    paths[dest]=1

    s.reverse()

    for i in s:
      for j in self.outbound(i):
        paths[i] = paths[i] + paths[j]

    print("Number of distinct paths from", source, "to", dest, "is", paths[source])

  def shortestPaths(self, source, dest):
    dist = [float("Inf")] * (self.__nrVertices)
    dist[source] = 0

    s = self.TopologicalSort()
    s.reverse()

    while s:
      i = s.pop()

      for node in self.outbound(i):
        if dist[node] > dist[i] + self.getCost((i, node)):
          dist[node] = dist[i] + self.getCost((i, node))

    count = [0] * self.__nrVertices

    DFS(self, source, count, dist)
    print("There are", count[dest], "distincts paths with the lowest cost from", source, "to", dest)

def DFS(graph, u, count, dist):
  for v in graph.outbound(u):
    if dist[v] == (dist[u] + graph.getCost((u,v))):
      count[v] = count[v]+1
      DFS(graph, v, count, dist)

def runUI():
  global graph
  
  print("> ", end="")
  command = input().split()

  cmd = command[0]
  parameters = command[1:]

  if(cmd == "exit"):
    exit()
  elif cmd == "text":
    with open(parameters[0], "r")as f:
      firstLine = f.readline()
      firstLine = firstLine.split()

      nrVertices = int(firstLine[0])
      nrEdges = int(firstLine[1])

    graph = DirectedGraph(nrVertices, nrEdges)
    graph.getGraphFromFile(parameters[0])
  elif cmd == "random":
    graph = DirectedGraph(int(parameters[0]), int(parameters[1]))
    graph = graph.randomGraph(int(parameters[0]),int(parameters[1]))

    graph.writeGraphToFile("randomgraph.txt")
  elif cmd == "print":
    graph.printGraph()
  elif cmd == "parse_vertices":
    print(graph.vertices())
  elif cmd == "parse_edges":
    print(graph.edges())
  elif cmd == "nr_vertices":
    print(graph.getNrVertices())
  elif cmd == "nr_edges":
    print(graph.getNrEdges())
  elif cmd == "is_edge":
    try:
      print(graph.isEdge(int(parameters[0]),int(parameters[1])))
    except Exception as e:
      print(e)
  elif cmd == "in_degree":
    print(graph.getInDegree(int(parameters[0])))
  elif cmd == "out_degree":
    print(graph.getOutDegree(int(parameters[0])))
  elif cmd == "parse_outbound":
    print(graph.parseOutboundEdges(int(parameters[0])))
  elif cmd == "parse_inbound":
    print(graph.parseInboundEdges(int(parameters[0])))
  elif cmd == "modify_cost":
    graph.modifyCost((int(parameters[0]), int(parameters[1])), int(parameters[2]))
    print("cost of " + str(parameters[0]) + " " + str(parameters[1]) + " is " + str(graph.getCost((int(parameters[0]), int(parameters[1])))))
  elif cmd == "add_edge":
    graph.addEdge(int(parameters[0]), int(parameters[1]), int(parameters[2]))
  elif cmd == "remove_edge":
    graph.removeEdge((int(parameters[0]), int(parameters[1])))
  elif cmd == "add_vertex":
    graph.addVertex(int(parameters[0]))
  elif cmd == "remove_vertex":
    graph.removeVertex(int(parameters[0]))
  elif cmd == "get_cost":
    print(graph.getCost((int(parameters[0]), int(parameters[1]))))
  elif cmd == "save_to_file":
    graph.writeGraphToFile(parameters[0])
  elif cmd == "topoSort":
    try:
      graph.TopologicalSort()
    except Exception as e:
      print(e)
  elif cmd == "schedule":
      graph.Schedule()
  elif cmd == "activities":
    maxVertex = 0
    with open(parameters[0], "r")as f:
      lines = f.readlines()
      for line in lines:
        if(int(line[0]) > maxVertex):
          maxVertex = int(line[0])

    graph = DirectedGraph(maxVertex+1, 0)
    graph.Activities(parameters[0])
  elif cmd == "nrPaths":
    graph.nrPaths(int(parameters[0]), int(parameters[1]))
  elif cmd == "shortestPaths":
    graph.shortestPaths(int(parameters[0]), int(parameters[1]))
  else:
    print("Command not recognized.")

def main():
  while True:
    try:
      runUI()
    except Exception as e:
      print(str(e))

if __name__ == "__main__":
  main()