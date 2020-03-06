#ifndef __GRAPH_HH__
#define __GRAPH_HH__

#include <functional>
#include <iosfwd>
#include <stdexcept>
#include <unordered_map>
#include <utility>
#include <vector>

// https://stackoverflow.com/a/32685618
struct PairHash {
  std::size_t operator ()(const std::pair<int, int>& p) const {
    auto h1 = std::hash<int>{}(p.first);
    auto h2 = std::hash<int>{}(p.second);

    return h1 ^ h2;  
  }
};

struct Graph {
  friend std::ostream& operator <<(std::ostream& os, const Graph& g);
  friend std::istream& operator >>(std::istream& is, Graph& g);

  Graph(int vertexCount) : _vertexCount{vertexCount}, _edgeCount{0}, _inbound{}, _outbound{}, _cost{} { }
  Graph() : Graph{0} { }

  int vertexCount() const {
    return _vertexCount;
  }

  void forEachVertex(std::function<void(int)> action) const {
    for (int i = 0; i < _vertexCount; ++i)
      action(i);
  }

  bool existsEdge(int vertex1, int vertex2) const {
    _assertInRange(vertex1);
    _assertInRange(vertex2);

    auto it = _outbound.find(vertex1);

    if (it == _outbound.end())
      return false;

    for (auto v : it->second)
      if (v == vertex2)
        return true;

    return false;
  }

  int inDegree(int vertex) const {
    _assertInRange(vertex);

    auto it = _inbound.find(vertex);

    if (it == _inbound.end())
      return 0;

    return it->second.size();
  }

  int outDegree(int vertex) const {
    _assertInRange(vertex);

    auto it = _outbound.find(vertex);

    if (it == _outbound.end())
      return 0;

    return it->second.size();
  }

  void forEachInbound(int vertex, std::function<void(int)> action) const {
    _assertInRange(vertex);

    auto it = _inbound.find(vertex);

    if (it == _inbound.end())
      return;

    for (auto v : it->second)
      action(v);
  }

  void forEachOutbound(int vertex, std::function<void(int)> action) const {
    _assertInRange(vertex);

    auto it = _outbound.find(vertex);

    if (it == _outbound.end())
      return;

    for (auto v : it->second)
      action(v);
  }

  int getCost(int vertex1, int vertex2) const {
    _assertInRange(vertex1);
    _assertInRange(vertex2);

    auto it = _cost.find({vertex1, vertex2});

    if (it == _cost.end())
      throw std::runtime_error{"Edge does not exist."};

    return it->second;
  }

  void setCost(int vertex1, int vertex2, int cost) {
    _assertInRange(vertex1);
    _assertInRange(vertex2);

    if (!existsEdge(vertex1, vertex2))
      throw std::runtime_error{"Edge does not exist."};

    _cost[{vertex1, vertex2}] = cost;
  }

  void addEdge(int vertex1, int vertex2, int cost) {
    _assertInRange(vertex1);
    _assertInRange(vertex2);

    if (existsEdge(vertex1, vertex2))
      throw std::runtime_error{"Edge already exists."};

    _outbound[vertex1].push_back(vertex2);
    _inbound[vertex2].push_back(vertex1);
    _cost[{vertex1, vertex2}] = cost;
    ++_edgeCount;
  }

  void removeEdge(int vertex1, int vertex2) {
    _assertInRange(vertex1);
    _assertInRange(vertex2);

    if (!existsEdge(vertex1, vertex2))
      throw std::runtime_error{"Edge does not exist."};

    auto& vo = _outbound[vertex1];
    vo.erase(std::find(vo.begin(), vo.end(), vertex2));

    auto& vi = _inbound[vertex2];
    vi.erase(std::find(vi.begin(), vi.end(), vertex1));

    _cost.erase({vertex1, vertex2});
    --_edgeCount;
  }

  void addVertex() {
    ++_vertexCount;
  }

  void removeVertex(int vertex) {
    _assertInRange(vertex);

    for (int i = 0; i < _vertexCount; ++i) {
      if (existsEdge(vertex, i))
        removeEdge(vertex, i);

      if (existsEdge(i, vertex))
        removeEdge(i, vertex);
    }

    for (int i = vertex + 1; i < _vertexCount; ++i) {
      for (int j = 0; j < _vertexCount; ++j) {
        if (existsEdge(i, j)) {
          int cost = getCost(i, j);
          removeEdge(i, j);
          addEdge(i - 1, j, cost);
        }
      }
    }

    for (int i = vertex + 1; i < _vertexCount; ++i) {
      for (int j = 0; j < _vertexCount; ++j) {
        if (existsEdge(j, i)) {
          int cost = getCost(j, i);
          removeEdge(j, i);
          addEdge(j, i - 1, cost);
        }
      }
    }

    --_vertexCount;
  }

private:
  void _assertInRange(int vertex) const {
    if (vertex < 0 || vertex >= _vertexCount)
      throw std::out_of_range{"Vertex out of range."};
  }

  int _vertexCount, _edgeCount;
  std::unordered_map<int, std::vector<int>> _inbound, _outbound;
  std::unordered_map<std::pair<int, int>, int, PairHash> _cost;
};

Graph randomGraph(int vertexCount, int edgeCount);

#endif