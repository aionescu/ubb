#ifndef UI_HH
#define UI_HH

#include <fstream>
#include <iostream>
#include <map>
#include <string>
#include <sstream>
#include <streambuf>
#include "Graph.hh"

class UI {
  Graph _graph;
  std::map<std::string, void (UI::*)(std::istringstream&)> _cmds;

public:
  UI() : _graph{}, _cmds{{
    { "help", &UI::help },
    { "exit", &UI::exit },
    { "vertex-count", &UI::vertexCount },
    { "vertices", &UI::vertices },
    { "is-vertex", &UI::isVertex },
    { "exists-edge", &UI::existsEdge },
    { "in-degree", &UI::inDegree },
    { "out-degree", &UI::outDegree },
    { "inbound", &UI::inbound },
    { "outbound", &UI::outbound },
    { "get-cost", &UI::getCost },
    { "set-cost", &UI::setCost },
    { "add-edge", &UI::addEdge },
    { "remove-edge", &UI::removeEdge },
    { "add-vertex", &UI::addVertex },
    { "remove-vertex", &UI::removeVertex },
    { "save-to-file", &UI::saveToFile },
    { "load-from-file", &UI::loadFromFile },
    { "load-old-fmt", &UI::loadOldFmt },
    { "print", &UI::print },
    { "random", &UI::random }
  }} {}

  void help(std::istringstream&) {
    std::cout << "Available commands:\n";

    for (auto kvp : _cmds) {
      std::cout << kvp.first << '\n';
    }
  }

  void exit(std::istringstream&) {
    std::exit(0);
  }

  void vertexCount(std::istringstream&) {
    std::cout << _graph.vertexCount() << '\n';
  }

  void isVertex(std::istringstream& args) {
    int v;
    args >> v;

    std::cout << _graph.isVertex(v) << '\n';
  }

  void vertices(std::istringstream&) {
    std::cout << "[ ";

    for (auto v : _graph.vertices())
      std::cout << v << ' ';

    std::cout << "]\n";
  }

  void existsEdge(std::istringstream& args) {
    int v1, v2;
    args >> v1 >> v2;

    std::cout << _graph.existsEdge(v1, v2) << '\n';
  }

  void inDegree(std::istringstream& args) {
    int v;
    args >> v;

    std::cout << _graph.inDegree(v) << '\n';
  }

  void outDegree(std::istringstream& args) {
    int v;
    args >> v;

    std::cout << _graph.outDegree(v) << '\n';
  }

  void inbound(std::istringstream& args) {
    int v;
    args >> v;

    std::cout << "[ ";

    for (auto v2 : _graph.inbound(v))
      std::cout << v2 << ' ';

    std::cout << "]\n";
  }

  void outbound(std::istringstream& args) {
    int v;
    args >> v;

    std::cout << "[ ";

    for (auto v2 : _graph.outbound(v))
      std::cout << v2 << ' ';

    std::cout << "]\n";
  }

  void getCost(std::istringstream& args) {
    int v1, v2;
    args >> v1 >> v2;

    std::cout << _graph.getCost(v1, v2) << '\n';
  }

  void setCost(std::istringstream& args) {
    int v1, v2, c;
    args >> v1 >> v2 >> c;

    _graph.setCost(v1, v2, c);
  }

  void addEdge(std::istringstream& args) {
    int v1, v2, c;
    args >> v1 >> v2 >> c;

    _graph.addEdge(v1, v2, c);
  }

  void removeEdge(std::istringstream& args) {
    int v1, v2;
    args >> v1 >> v2;

    _graph.removeEdge(v1, v2);
  }

  void addVertex(std::istringstream& args) {
    int v;
    args >> v;

    _graph.addVertex(v);
  }

  void removeVertex(std::istringstream& args) {
    int v;
    args >> v;

    _graph.removeVertex(v);
  }

  void saveToFile(std::istringstream& args) {
    std::string s;
    args >> s;

    std::ofstream{s} << _graph;
  }

  void loadFromFile(std::istringstream& args) {
    std::string s;
    args >> s;

    std::ifstream{s} >> _graph;
  }

  void loadOldFmt(std::istringstream& args) {
    std::string s;
    args >> s;

    std::ifstream is{s};
    _graph = Graph::fromStreamOld(is);
  }

  void print(std::istringstream&) {
    std::cout << _graph;
  }

  void random(std::istringstream& args) {
    int vertexCount, edgeCount;
    args >> vertexCount >> edgeCount;

    _graph = Graph::randomGraph(vertexCount, edgeCount);
  }

  void handleCommand() {
    std::string s;
    std::getline(std::cin, s);
    std::istringstream ss{s};

    ss >> s;

    auto cmd = _cmds.find(s);

    if (cmd == _cmds.end()) {
      std::cout << "Command not recognized.\n";
      return;
    }
    
    try {
      auto fPtr = cmd->second;
      (this->*fPtr)(ss);
    } catch (std::exception& e) {
      std::cout << "Error: " << e.what() << "\n";
    }
  }
};

#endif