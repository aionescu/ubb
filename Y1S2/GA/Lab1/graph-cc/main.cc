#include <fstream>
#include <iostream>
#include "Graph.hh"
#include "UI.hh"

int main() {
  std::cout << std::boolalpha;

  UI ui;

  while (true) {
    std::cout << "> ";
    ui.handleCommand();
  }
}