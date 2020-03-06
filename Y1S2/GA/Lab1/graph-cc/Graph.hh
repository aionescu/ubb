#ifndef __GRAPH_HH__
#define __GRAPH_HH__

#include <functional>
#include <optional>
#include <stdexcept>
#include <unordered_map>
#include <utility>
#include <vector>

struct Graph {
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

  std::optional<int> getCost(int vertex1, int vertex2) const {
    _assertInRange(vertex1);
    _assertInRange(vertex2);

    auto it = _cost.find({vertex1, vertex2});

    if (it == _cost.end())
      return {};

    return {it->second};
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
  }

  void addVertex() {
    ++_vertexCount;
  }

  void removeVertex(int vertex) {
    _assertInRange(vertex);

    throw std::runtime_error{"Unimplemented."};
  }
  
private:
  void _assertInRange(int vertex) const {
    if (vertex < 0 || vertex >= _vertexCount)
      throw std::out_of_range{"Vertex out of range."};
  }

  int _vertexCount;
  std::unordered_map<int, std::vector<int>> _inbound, _outbound;
  std::unordered_map<std::pair<int, int>, int> _cost;
};

#endif