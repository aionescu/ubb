#include <assert.h>
#include "Matrix.h"
#include <iostream>

using namespace std;

void testNumberOfNonZeroElems() {
  std::cout << "Test numberOfNonZeroElems" << std::endl;

  Matrix m{10, 10};

  try {
    m.numberOfNonZeroElems(-1);
    assert(false);
  } catch (...) {
    assert(true);
  }

  assert(m.numberOfNonZeroElems(0) == 0);

  m.modify(0, 0, 1);
  m.modify(1, 0, 1);
  m.modify(2, 0, 1);

  assert(m.numberOfNonZeroElems(0) == 3);
}

void testAll() { 
  testNumberOfNonZeroElems();

  Matrix m(4, 4);
  assert(m.nrLines() == 4);
  assert(m.nrColumns() == 4);  
  m.modify(1, 1, 5);
  assert(m.element(1, 1) == 5);
  TElem old = m.modify(1, 1, 6);
  assert(m.element(1, 2) == NULL_TELEM);
  assert(old == 5);
}