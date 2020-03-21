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
  std::map<std::string, void (UI::*)(std::stringstream&)> _cmds;

public:
  UI() : _graph{} {
    _cmds["help"] = &UI::help;
    _cmds["exit"] = &UI::exit;
    _cmds["vertex-count"] = &UI::vertexCount;
    _cmds["vertices"] = &UI::vertices;
    _cmds["is-vertex"] = &UI::isVertex;
    _cmds["exists-edge"] = &UI::existsEdge;
    _cmds["in-degree"] = &UI::inDegree;
    _cmds["out-degree"] = &UI::outDegree;
    _cmds["inbound"] = &UI::inbound;
    _cmds["outbound"] = &UI::outbound;
    _cmds["get-cost"] = &UI::getCost;
    _cmds["set-cost"] = &UI::setCost;
    _cmds["add-edge"] = &UI::addEdge;
    _cmds["remove-edge"] = &UI::removeEdge;
    _cmds["add-vertex"] = &UI::addVertex;
    _cmds["remove-vertex"] = &UI::removeVertex;
    _cmds["save-to-file"] = &UI::saveToFile;
    _cmds["load-from-file"] = &UI::loadFromFile;
    _cmds["load-old-fmt"] = &UI::loadOldFmt;
    _cmds["print"] = &UI::print;
    _cmds["random"] = &UI::random;
  }

  void help(std::stringstream& args) {
    std::cout << "Available commands:\n";

    for (auto kvp : _cmds) {
      std::cout << kvp.first << '\n';
    }
  }

  void exit(std::stringstream& args) {
    std::exit(0);
  }

  void vertexCount(std::stringstream& args) {
    std::cout << _graph.vertexCount() << '\n';
  }

  void isVertex(std::stringstream& args) {
    int v;
    args >> v;

    std::cout << _graph.isVertex(v) << '\n';
  }

  void vertices(std::stringstream& args) {
    std::cout << "[ ";

    for (auto v : _graph.vertices())
      std::cout << v << ' ';

    std::cout << "]\n";
  }

  void existsEdge(std::stringstream& args) {
    int v1, v2;
    args >> v1 >> v2;

    std::cout << _graph.existsEdge(v1, v2) << '\n';
  }

  void inDegree(std::stringstream& args) {
    int v;
    args >> v;

    std::cout << _graph.inDegree(v) << '\n';
  }

  void outDegree(std::stringstream& args) {
    int v;
    args >> v;

    std::cout << _graph.outDegree(v) << '\n';
  }

  void inbound(std::stringstream& args) {
    int v;
    args >> v;

    std::cout << "[ ";

    for (auto v2 : _graph.inbound(v))
      std::cout << v2 << ' ';

    std::cout << "]\n";
  }

  void outbound(std::stringstream& args) {
    int v;
    args >> v;

    std::cout << "[ ";

    for (auto v2 : _graph.outbound(v))
      std::cout << v2 << ' ';

    std::cout << "]\n";
  }

  void getCost(std::stringstream& args) {
    int v1, v2;
    args >> v1 >> v2;

    std::cout << _graph.getCost(v1, v2) << '\n';
  }

  void setCost(std::stringstream& args) {
    int v1, v2, c;
    args >> v1 >> v2 >> c;

    _graph.setCost(v1, v2, c);
  }

  void addEdge(std::stringstream& args) {
    int v1, v2, c;
    args >> v1 >> v2 >> c;

    _graph.addEdge(v1, v2, c);
  }

  void removeEdge(std::stringstream& args) {
    int v1, v2;
    args >> v1 >> v2;

    _graph.removeEdge(v1, v2);
  }

  void addVertex(std::stringstream& args) {
    int v;
    args >> v;

    _graph.addVertex(v);
  }

  void removeVertex(std::stringstream& args) {
    int v;
    args >> v;

    _graph.removeVertex(v);
  }

  void saveToFile(std::stringstream& args) {
    std::string s;
    args >> s;

    std::ofstream{s} << _graph.toString();
  }

  void loadFromFile(std::stringstream& args) {
    std::string s;
    args >> s;

    std::ifstream t{s};
    std::string str;

    t.seekg(0, std::ios::end);   
    str.reserve(t.tellg());
    t.seekg(0, std::ios::beg);

    str.assign((std::istreambuf_iterator<char>(t)), std::istreambuf_iterator<char>());

    _graph = Graph::fromString(str);
  }

  void loadOldFmt(std::stringstream& args) {
    std::string s;
    args >> s;

    std::ifstream t{s};
    std::string str;

    t.seekg(0, std::ios::end);   
    str.reserve(t.tellg());
    t.seekg(0, std::ios::beg);

    str.assign((std::istreambuf_iterator<char>(t)), std::istreambuf_iterator<char>());

    _graph = Graph::fromStringOld(str);
  }

  void print(std::stringstream& args) {
    std::cout << _graph.toString();
  }

  void random(std::stringstream& args) {
    int vertexCount, edgeCount;
    args >> vertexCount >> edgeCount;

    _graph = Graph::randomGraph(vertexCount, edgeCount);
  }

  void handleCommand() {
    std::string s;
    std::getline(std::cin, s);
    std::stringstream ss{s};

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
      std::cout << "Exception: " << e.what() << "\n";
    }
  }
};

#endif