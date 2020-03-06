#include <cstdlib>
#include <ctime>
#include <iostream>
#include <stdexcept>
#include "Graph.hh"

std::ostream& operator <<(std::ostream& os, const Graph& g) {
  os << g._vertexCount << ' ' << g._edgeCount << '\n';

  g.forEachVertex([&](int v1) {
    g.forEachOutbound(v1, [&](int v2) {
      os << v1 << ' ' << v2 << ' ' << g.getCost(v1, v2) << '\n';
    });
  });

  return os;
}

std::istream& operator >>(std::istream& is, Graph& g) {
  g.~Graph();
  new (&g) Graph();

  int edgeCount;

  is >> g._vertexCount >> edgeCount;

  for (int i = 0; i < edgeCount; ++i) {
    int v1, v2, cost;
    is >> v1 >> v2 >> cost;

    g.addEdge(v1, v2, cost);
  }

  return is;
}

int rand_range(int min, int max) {
  return std::rand() % (max - min) + min;
}

Graph randomGraph(int vertexCount, int edgeCount) {
  std::srand(std::time(0));
  Graph g{vertexCount};

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