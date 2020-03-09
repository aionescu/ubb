#ifndef GRAPH_HH
#define GRAPH_HH

#include <algorithm>
#include <cstdlib>
#include <ctime>
#include <functional>
#include <map>
#include <sstream>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

static inline int rand_range(int min, int max) {
  return std::rand() % (max - min) + min;
}

class Graph {
public:
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

  static Graph fromString(std::string s) {
    std::stringstream ss{s};
    Graph g;

    int v1, v2, c;

    while (ss >> v1 >> v2) {
      if (v2 == -1) {
        g.addVertex(v1);
        continue;
      }

      ss >> c;

      if (!g.isVertex(v1))
        g.addVertex(v1);

      if (!g.isVertex(v2))
        g.addVertex(v2);

      g.addEdge(v1, v2, c);
    }

    return g;
  }

  static Graph fromStringOld(std::string s) {
    std::stringstream ss{s};
    Graph g;

    int vertexCount, edgeCount;

    ss >> vertexCount >> edgeCount;

    for (int i = 0; i < vertexCount; ++i)
      g.addVertex(i);
      
    for (int i = 0; i < edgeCount; ++i) {
      int v1, v2, cost;
      ss >> v1 >> v2 >> cost;

      g.addEdge(v1, v2, cost);
    }

    return g;
  }

  std::string toString() const {
    std::stringstream ss{};

    for (auto v1 : vertices()) {
      auto outbound = this->outbound(v1);
      auto inbound = this->inbound(v1);

      if (outbound.size() == 0 && inbound.size() == 0)
        ss << v1 << " -1" << '\n';
      else {
        for (auto v2 : outbound)
          ss << v1 << ' ' << v2 << ' ' << getCost(v1, v2) << '\n';
      }
    }

    return ss.str();
  }

  int vertexCount() const {
    return _outbound.size();
  }

  bool isVertex(int vertex) const {
    return _outbound.find(vertex) != _outbound.end();
  }

  std::vector<int> vertices() const {
    std::vector<int> vs;

    std::transform(_outbound.begin(), _outbound.end(), std::back_inserter(vs),
      [&](auto it) { return it.first; });

    return vs;
  }

  bool existsEdge(int vertex1, int vertex2) const {
    _assertVertexExists(vertex1);
    _assertVertexExists(vertex2);

    auto v = _outbound.at(vertex1);
    return std::count(v.begin(), v.end(), vertex2) > 0;
  }

  int inDegree(int vertex) const {
    _assertVertexExists(vertex);

    return _inbound.at(vertex).size();
  }

  int outDegree(int vertex) const {
    _assertVertexExists(vertex);

    return _outbound.at(vertex).size();
  }

  std::vector<int> inbound(int vertex) const {
    _assertVertexExists(vertex);

    return std::vector<int>{_inbound.at(vertex)};
  }

  std::vector<int> outbound(int vertex) const {
    _assertVertexExists(vertex);

    return std::vector<int>{_outbound.at(vertex)};
  }

  int getCost(int vertex1, int vertex2) const {
    _assertVertexExists(vertex1);
    _assertVertexExists(vertex2);

    if (!existsEdge(vertex1, vertex2))
      throw std::runtime_error{"Edge does not exist."};

    return _cost.at({vertex1, vertex2});
  }

  void setCost(int vertex1, int vertex2, int cost) {
    _assertVertexExists(vertex1);
    _assertVertexExists(vertex2);

    if (!existsEdge(vertex1, vertex2))
      throw std::runtime_error{"Edge does not exist."};

    _cost[{vertex1, vertex2}] = cost;
  }

  void addEdge(int vertex1, int vertex2, int cost) {
    _assertVertexExists(vertex1);
    _assertVertexExists(vertex2);

    if (existsEdge(vertex1, vertex2))
      throw std::runtime_error{"Edge already exists."};

    _outbound[vertex1].push_back(vertex2);
    _inbound[vertex2].push_back(vertex1);
    _cost[{vertex1, vertex2}] = cost;
  }

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

  void addVertex(int vertex) {
    if (isVertex(vertex))
      throw std::runtime_error{"Vertex already exists."};

    _outbound[vertex] = {};
    _inbound[vertex] = {};
  }

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

private:
  void _assertVertexExists(int vertex) const {
    if (!isVertex(vertex))
      throw std::out_of_range{"Vertex does not exist."};
  }

  std::map<int, std::vector<int>> _inbound, _outbound;
  std::map<std::pair<int, int>, int> _cost;
};

#endif
