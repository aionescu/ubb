from domain import *

graph = Graph()
fileReadGraph(graph, "Graph.txt")
sourceVertex = 0

distances, previous = dijkstra(graph, sourceVertex)

for destVertex in graph.parse():
  if destVertex != sourceVertex:
    if destVertex not in distances:
      print("There is no path from", letters[sourceVertex], "to", letters[destVertex])
    else:
      print("Path from", letters[sourceVertex], "to", letters[destVertex], "is:", list(map(lambda x: letters[x], getPath(previous, sourceVertex, destVertex))))
      print("Distance:", distances[destVertex])
      print()
