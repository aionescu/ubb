#ifndef __UI_H__
#define __UI_H__

#include <fstream>
#include <functional>
#include <iostream>
#include <string>
#include <sstream>
#include <unordered_map>
#include "Graph.hh"

#define CMD(s, f) _cmds[s] = [&](std::stringstream& args) { this->f(args); }

struct UI {
  UI() : _graph{} {
    CMD("help", help);
    CMD("exit", exit);
    CMD("vertex-count", vertexCount);
    CMD("exists-edge", existsEdge);
    CMD("in-degree", inDegree);
    CMD("out-degree", outDegree);
    CMD("get-cost", getCost);
    CMD("set-cost", setCost);
    CMD("add-edge", addEdge);
    CMD("remove-edge", removeEdge);
    CMD("add-vertex", addVertex);
    CMD("remove-vertex", removeVertex);
    CMD("save-to-file", saveToFile);
    CMD("load-from-file", loadFromFile);
    CMD("print", print);
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
    _graph.addVertex();
  }

  void removeVertex(std::stringstream& args) {
    int v;
    args >> v;

    _graph.removeVertex(v);
  }

  void saveToFile(std::stringstream& args) {
    std::string s;
    args >> s;

    std::ofstream{s} << _graph;
  }

  void loadFromFile(std::stringstream& args) {
    std::string s;
    args >> s;

    std::ifstream{s} >> _graph;
  }

  void print(std::stringstream& args) {
    std::cout << _graph;
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
      cmd->second(ss);
    } catch (std::exception& e) {
      std::cout << "Exception: " << e.what() << "\n";
    }
  }

private:
  Graph _graph;
  std::unordered_map<std::string, std::function<void(std::stringstream&)>> _cmds;
};

#endif