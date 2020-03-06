#include <fstream>
#include <iostream>
#include "Graph.hh"

int main() {
  Graph g;
  std::ifstream{"digraph-ex1.txt"} >> g;

  std::cout << g << '\n';
  std::cout << randomGraph(5, 10);
}