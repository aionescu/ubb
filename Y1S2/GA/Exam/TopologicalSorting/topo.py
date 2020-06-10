#Python program to print topological sorting of a DAG 
from collections import defaultdict 
  
letters = ["A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z"]

#Class to represent a graph 
class Graph: 
    def __init__(self,vertices): 
        self.graph = defaultdict(list) #dictionary containing adjacency List 
        self.V = vertices #No. of vertices 
  
    # function to add an edge to graph 
    def addEdge(self,u,v): 
        self.graph[u].append(v) 
  
    # A recursive function used by topologicalSort 
    def topologicalSortUtil(self,v,visited,stack, execStack): 
        execStack.append(letters[v])

        print("Current vertex:", letters[v])
        print("Stack on entry:", execStack)
        print()

        # Mark the current node as visited. 
        visited[v] = True
  
        # Recur for all the vertices adjacent to this vertex 
        for i in self.graph[v]: 
            if visited[i] == False: 
                self.topologicalSortUtil(i,visited,stack, execStack) 

        # Push current vertex to stack which stores result 
        stack.insert(0,letters[v])
        print("Sorted list so far:", stack)
        
        execStack.pop()
        print("Stack on exit:", execStack)
        print()
  
    # The function to do Topological Sort. It uses recursive  
    # topologicalSortUtil() 
    def topologicalSort(self): 
        # Mark all the vertices as not visited 
        visited = [False]*self.V 
        stack =[] 
        execStack = []

        # Call the recursive helper function to store Topological 
        # Sort starting from all vertices one by one 
        for i in range(self.V): 
            if visited[i] == False:
                self.topologicalSortUtil(i,visited,stack, execStack) 
  
        # Print contents of the stack 
        print("Topological sorting:", stack)
  
g= Graph(7)

g.addEdge(0, 1)
g.addEdge(0, 2)
g.addEdge(0, 6)
g.addEdge(1, 2)
g.addEdge(1, 5)
g.addEdge(2, 3)
g.addEdge(3, 4)
g.addEdge(3, 5)
g.addEdge(4, 5)
g.addEdge(4, 6)
g.addEdge(6, 5)

g.topologicalSort() 
#This code is contributed by Neelam Yadav 
