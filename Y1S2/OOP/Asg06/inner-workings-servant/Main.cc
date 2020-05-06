#include <iostream>
#include "UI.hh"

int main() {
  UI ui{std::cin, std::cout};
  ui.mainLoop();
}