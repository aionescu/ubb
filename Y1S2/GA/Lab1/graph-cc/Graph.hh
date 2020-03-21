#ifndef GRAPH_HH
#define GRAPH_HH

#include <algorithm>
#include <cstdlib>
#include <ctime>
#include <functional>
#include <iostream>
#include <map>
#include <sstream>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

static inline int rand_range(int min, int max) {
  return std::rand() % (max - min) + min;
}

// Class that represents a directed graph.
class Graph {
  // Representation of the graph:
  // * The graph is represented by 3 ordered dictionaries (std::map),
  //   one that stores the inbound edges for each vertex, one that stores
  //   the outbound edges for each vertex, and one that stores the cost
  //   associated to each edge.
  // * The existence of a vertex can be checked by simply checking if it
  //   exists as a key in either the `_inbound` or `_outbound` dictionaries.
  // * The existence of an edge can be checked by simply checking if it exists
  //   as a key in the `_cost` dictionary.
  // * The Graph class contains a default constructor, copy constructor and
  //   move constructor, so copying the data from one graph to another is as
  //   easy as writing `Graph g2 = g;`.
  std::map<int, std::vector<int>> _inbound, _outbound;
  std::map<std::pair<int, int>, int> _cost;

  void _assertVertexExists(int vertex) const {
    if (!isVertex(vertex))
      throw std::out_of_range{"Vertex does not exist."};
  }

public:
  // Creates a graph with `vertexCount` vertices numbered from 0
  // to `vertexCount` - 1, and `edgeCount` randomly generated edges.
  static Graph randomGraph(int vertexCount, int edgeCount) {
    std::srand(std::time(0));
    Graph g;

    for (int i = 0; i < vertexCount; ++i)
      g.addVertex(i);

    for (int i = 0; i < edgeCount;) {
      int v1 = rand_range(0, vertexCount);
      int v2 = rand_range(0, vertexCount);
      int cost = rand_range(-100, 100);

      try {
        g.addEdge(v1, v2, cost);
        ++i;
      } catch (...) {

      }
    }

    return g;
  }

  // Reads a graph from the given stream, which is expected
  // to be in the "old" format (that assumes the graph contains all
  // vertices from 0 to n - 1).
  static Graph fromStreamOld(std::istream& is) {
    Graph g;

    int vertexCount, edgeCount;

    is >> vertexCount >> edgeCount;

    for (int i = 0; i < vertexCount; ++i)
      g.addVertex(i);
      
    for (int i = 0; i < edgeCount; ++i) {
      int v1, v2, cost;
      is >> v1 >> v2 >> cost;

      g.addEdge(v1, v2, cost);
    }

    return g;
  }

  // Returns the number of vertices in the graph.
  int vertexCount() const {
    return _outbound.size();
  }

  // Checks whether the graph contains the specified vertex.
  // Returns: `true` if the vertex exists in the graph, otherwise `false`.
  bool isVertex(int vertex) const {
    return _outbound.find(vertex) != _outbound.end();
  }

  // Returns a vector containing all vertices in the graph, in ascending order.
  std::vector<int> vertices() const {
    std::vector<int> vs;

    std::transform(_outbound.begin(), _outbound.end(), std::back_inserter(vs),
      [&](auto it) { return it.first; });

    return vs;
  }

  // Checks whether there exists an edge between the 2 vertices.
  // Returns: `true` if there exists an edge between the 2 vertices,
  // otherwise `false`.
  // Throws: std::out_of_range if either of the vertices is not in the graph.
  bool existsEdge(int vertex1, int vertex2) const {
    _assertVertexExists(vertex1);
    _assertVertexExists(vertex2);

    return _cost.find({vertex1, vertex2}) != _cost.end();
  }

  // Returns the in degree of the specified vertex.
  // Throws: std::out_of_range if the specified vertex is not in the graph.
  int inDegree(int vertex) const {
    _assertVertexExists(vertex);

    return _inbound.at(vertex).size();
  }

  // Returns the out degree of the specified vertex.
  // Throws: std::out_of_range if the specified vertex is not in the graph.
  int outDegree(int vertex) const {
    _assertVertexExists(vertex);

    return _outbound.at(vertex).size();
  }

  // Returns a vector containing all the inbound edges of the specified vertex.
  // Throws: std::out_of_range if the specified vertex is not in the graph.
  // Law: forall v. forall v2 in inbound(v). existsEdge(v2, v)
  const std::vector<int>& inbound(int vertex) const {
    _assertVertexExists(vertex);

    return _inbound.at(vertex);
  }

  // Returns a vector containing all the outbound edges of the specified vertex.
  // Throws: std::out_of_range if the specified vertex is not in the graph.
  // Law: forall v. forall v2 in outbound(v). existsEdge(v, v2)
  const std::vector<int>& outbound(int vertex) const {
    _assertVertexExists(vertex);

    return _outbound.at(vertex);
  }

  // Returns the cost associated to the edge between `vertex1` and `vertex2`.
  // Throws:
  //   * std::out_of_range if either vertex is not in the graph.
  //   * std::runtime_error if there is no edge between vertex1 and vertex2.
  int getCost(int vertex1, int vertex2) const {
    _assertVertexExists(vertex1);
    _assertVertexExists(vertex2);

    if (!existsEdge(vertex1, vertex2))
      throw std::runtime_error{"Edge does not exist."};

    return _cost.at({vertex1, vertex2});
  }

  // Sets the cost associated to the edge between `vertex1` and `vertex2` to
  // be equal to `cost`.
  // Throws:
  //   * std::out_of_range if either vertex is not in the graph.
  //   * std::runtime_error if there is no edge between vertex1 and vertex2.
  void setCost(int vertex1, int vertex2, int cost) {
    _assertVertexExists(vertex1);
    _assertVertexExists(vertex2);

    if (!existsEdge(vertex1, vertex2))
      throw std::runtime_error{"Edge does not exist."};

    _cost[{vertex1, vertex2}] = cost;
  }

  // Adds a new edge between `vertex1` and `vertex2`, with the cost equal
  // to `cost`.
  // Throws:
  //   * std::out_of_range if either vertex is not in the graph.
  //   * std::runtime_error if there already exists an edge between
  //     vertex1 and vertex2.
  void addEdge(int vertex1, int vertex2, int cost) {
    _assertVertexExists(vertex1);
    _assertVertexExists(vertex2);

    if (existsEdge(vertex1, vertex2))
      throw std::runtime_error{"Edge already exists."};

    _outbound[vertex1].push_back(vertex2);
    _inbound[vertex2].push_back(vertex1);
    _cost[{vertex1, vertex2}] = cost;
  }

  // Removes the edge between `vertex1` and `vertex2`.
  // Throws:
  //   * std::out_of_range if either vertex is not in the graph.
  //   * std::runtime_error if there is no edge between vertex1 and vertex2.
  void removeEdge(int vertex1, int vertex2) {
    _assertVertexExists(vertex1);
    _assertVertexExists(vertex2);

    if (!existsEdge(vertex1, vertex2))
      throw std::runtime_error{"Edge does not exist."};

    auto& vo = _outbound[vertex1];
    vo.erase(std::find(vo.begin(), vo.end(), vertex2));

    auto& vi = _inbound[vertex2];
    vi.erase(std::find(vi.begin(), vi.end(), vertex1));

    _cost.erase({vertex1, vertex2});
  }

  // Adds the specified vertex to the graph.
  // Throws: std::runtime_error if the vertex already exists.
  void addVertex(int vertex) {
    if (isVertex(vertex))
      throw std::runtime_error{"Vertex already exists."};

    _outbound[vertex] = {};
    _inbound[vertex] = {};
  }

  // Removes the specified vertex from the graph.
  // Throws: std::runtime_error if the vertex is not in the graph.
  void removeVertex(int vertex) {
    _assertVertexExists(vertex);

    for (auto v2 : _outbound[vertex]) {
      auto& v = _inbound[v2];

      v.erase(std::find(v.begin(), v.end(), vertex));
      _cost.erase({vertex, v2});
    }

    _outbound.erase(vertex);

    for (auto v2 : _inbound[vertex]) {
      auto& v = _outbound[v2];

      v.erase(std::find(v.begin(), v.end(), vertex));
      _cost.erase({v2, vertex});
    }

    _inbound.erase(vertex);
  }
};

// Reads a graph from the given stream.
inline std::istream& operator >>(std::istream& is, Graph& g) {
  g.~Graph();
  new (&g) Graph();

  int v1, v2, c;

  while (is >> v1 >> v2) {
    if (v2 == -1) {
      g.addVertex(v1);
      continue;
    }

    is >> c;

    if (!g.isVertex(v1))
      g.addVertex(v1);

    if (!g.isVertex(v2))
      g.addVertex(v2);

    g.addEdge(v1, v2, c);
  }

  return is;
}

// Writes the graph to the stream.
inline std::ostream& operator <<(std::ostream& os, const Graph& g) {
  for (auto v1 : g.vertices())
    if (g.inDegree(v1) == 0 && g.outDegree(v1) == 0)
      os << v1 << " -1" << '\n';
    else
      for (auto v2 : g.outbound(v1))
        os << v1 << ' ' << v2 << ' ' << g.getCost(v1, v2) << '\n';

  return os;
}

#endif
