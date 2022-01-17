#include <cassert>
#include "Services.hh"
#include "Test.hh"

void test_Service_tryAddStar() {
  Service service;
  service.loadFromFile<Star>("../Stars.txt");

  try {
    service.tryAddStar("", "const", 1, 1, 1);
    assert(false);
  } catch (InvalidStar&) {
    assert(true);
  }

  try {
    service.tryAddStar("star", "const", 1, 1, 0);
    assert(false);
  } catch (InvalidStar&) {
    assert(true);
  }

  service.tryAddStar("star", "const", 1, 1, 1);

  try {
    service.tryAddStar("star", "const", 1, 1, 1);
    assert(false);
  } catch (InvalidStar&) {
    assert(true);
  }

  std::cout << "All tests passed." << std::endl;
}