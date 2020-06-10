from queue import PriorityQueue

class Graph:
    def __init__(self, n = 0):
        self._InBound = {}
        self._OutBound = {}
        self._Cost = {}
        self._Vertices = n
        self._Edges = 0;

        for i in range(0, n):
            self._InBound[i] = []
            self._OutBound[i] = []

    def getNumberOfVertices(self):
        return self._Vertices

    def getNumberOfEdges(self):
        return self._Edges

    def setNumberOfVertices(self, n, reset = False):
        self._Vertices = n;
        if reset:
            for i in range(0, n):
                    self._InBound[i] = []
                    self._OutBound[i] = []

    def parse(self):
        # Returns a iterable list containing the indexes of the edges: [0,1,...,n-1]
        return list(self._InBound.keys())

    def parseInBound(self, x):
        # Returns a list(copy) containing the target vertices from vertex x.
        if self.isVertex(x):
            return self._InBound[x]
        #return False

    def parseOutBound(self, x):
        # Returns a list(copy) containing the source vertices into vertex x.
        if self.isVertex(x):
            return list(self._OutBound[x])
        return False

    def inDegree(self, x):
        return len(self._InBound[x])

    def outDegree(self, x):
        return len(self._OutBound[x])

    def addEdge(self, x, y, c):
        # Adds a directed edge from x to y, having the cost c
        if not self.isEdge(x, y):
            self._InBound[y].append(x)
            self._OutBound[x].append(y)
            self._Cost[(x, y)] = c
            self._Edges += 1
            return True
        return False

    def isEdge(self, x, y):
        # Searches if there exists an edge from x to y.
        # if x in self._OutBound:
        return y in self._OutBound[x]
        # else:
            # return False

        # looks for edge x->y
        # for i in self._OutBound[x]:
        #     if i == y:
        #         return True
        # return False

    def removeEdge(self, x, y):
        # Removes the edge x -> y.
        if self.isEdge(x, y):
            self._InBound[y].remove(x)
            self._OutBound[x].remove(y)
            del self._Cost[(x, y)]
            self._Edges -= 1
            return True
        return False

    def addVertex(self, x):
        # Adds a new vertex, having the index x.
        if not self.isVertex(x):
            self._InBound[x] = []
            self._OutBound[x] = []
            self._Vertices += 1
            return True
        return False

    def isVertex(self, x):
        # Checks wheter the vertex x exists or not.
        if x in self.parse():
            return True
        return False

    def removeVertex(self, x):
        # Removes the vertex x if it exists.
        if self.isVertex(x):
            for i in self.parseInBound(x):
                self.removeEdge(i, x)
            for i in self.parseOutBound(x):
                self.removeEdge(x, i)
            del self._InBound[x]
            del self._OutBound[x]
            return True
        return False

    def getCost(self, x, y):
        if self.isEdge(x, y):
            return self._Cost[(x, y)]
        return False

    def setCost(self, x, y, new_cost):
        if self.isEdge(x, y):
            self._Cost[(x, y)] = new_cost
            return True
        return False

    def printGraph(self):
        # Formatted printing of the graph on the screen.
        # Form: m -> n, with the cost c
        #       q is an isolated vertex
        for i in self.parse():
            if self.inDegree(i) == self.outDegree(i) == 0:
                print(i, "is an isolated vertex")
            else:
                for j in self._OutBound[i]:
                    print(i, "->", j, "having the cost:", self._Cost[(i, j)])

    def copy(self):
        # Creeates a new Graph instance, independent of this one, that can be modified.

        newGraph = Graph(0)

        newGraph._InBound = dict(self._InBound)
        newGraph._OutBound = dict(self._OutBound)
        newGraph._Cost = dict(self._Cost)
        newGraph._Vertices = self._Vertices

        return newGraph


   
    def printGraphInFile(self,filename):
        # formatted, more readable printing
        # ex:
        #   1 -> 99, having the cost 176
        #   9 is a isolated vertex
        f = open(filename,"w+")
        for i in self.parse():
            if len(self._InBound[i]) == len(self._OutBound[i]) == 0:
                f.write("\n")
                f.write(str(i))
                f.write(" is an isolated vertex")
            else:
                for j in self._OutBound[i]:
                   # f.write(i, "->", j, "having the cost:", self.Cost[(i, j)])
                    f.write("\n")
                    f.write(str(i))
                    f.write(" -> ")
                    f.write(str(j))
                    f.write(" having the cost: ")
                    f.write(str(self._Cost[(i,j)]))
        f.close()


  

def fileReadGraph(G, filename):
    # Reads the content of filename into the Graph G.

    f = open(filename, "r")

    if not f:
        return False

    lines = f.readline().split("\n")
    G.setNumberOfVertices(int(lines[0].split(" ")[0]), True)

    lines = f.readline().strip()
    while lines != "":
        line = lines.split(" ")
        x = int(line[0])
        y = int(line[1])
        c = int(line[2])
        G.addEdge(x, y, c)
        lines = f.readline().strip()

    return True


def filePrintGraph(G, filename):
    # Saves the information contained in Graph G into filename

    f = open(filename, "w+")

    if not f:
        return False

    f.write(str(G.getNumberOfVertices()) +" " + str(G.getNumberOfEdges()))

    for i in G.parse():
        # isolated vertex, skip it
        if G.inDegree(i) == G.outDegree(i) == 0:
            pass
        else:
            for j in G.parseOutBound(i):
                f.write("\n")
                f.write(str(i)+" ")
                f.write(str(j)+" ")
                f.write(str(G.getCost(i,j)))
    f.close()

letters = ["A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z"]

def letterify(dict):
  d = {}

  for item in dict.items():
    d[letters[item[0]]] = item[1]

  return d

def letterifyAll(dict):
  d = {}

  for item in dict.items():
    d[letters[item[0]]] = letters[item[1]]

  return d

def letterifyQueue(queue):
  l = []

  for pair in queue:
    l.append((pair[0], letters[pair[1]]))

  return l

# Considers the cost as the distance between two vertices.
def dijkstra(G, src):
    q = PriorityQueue()
    prev = {}
    dist = {}

    q.put((0, src))
    dist[src] = 0

    while not q.empty():
        x = q.get()[1]

        for y in G.parseOutBound(x):
            if y not in dist.keys() or dist[x] + G.getCost(x,y) < dist[y]:
                dist[y] = dist[x] + G.getCost(x,y)
                q.put((dist[y], y))
                prev[y] = x

        print("Current vertex:", letters[x])
        print("Queue:", letterifyQueue(q.queue))
        print("Distances:", letterify(dist))
        print("Previous:", letterifyAll(prev))
        print()

    return((dist, prev))

def backwardsDijkstra(G, dest):
    q = PriorityQueue()
    prev = {}
    dist = {}

    q.put((0, dest))
    dist[dest] = 0

    while not q.empty():
        x = q.get()[1]
        for y in G.parseInBound(x):
            if y not in dist.keys() or dist[x] + G.getCost(x,y) < dist[y]:
                dist[y] = dist[x] + G.getCost(x,y)
                q.put((dist[y], y))
                prev[y] = x

    return((dist, prev))



# Builds the path from vertex src to vetrex dest using the prev dictionary generated by djkstra's algorithm.
def getPath(prev, src, dst):

    path = [dst]

    curr = dst
    while(curr != src):
        curr = prev[curr]
        path.insert(0, curr)

    return path












