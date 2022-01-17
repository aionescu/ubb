#include <assert.h>

#include "SortedMap.h"
#include "SMIterator.h"
#include "ShortTest.h"
#include <exception>
#include <stdexcept>
#include <iostream>

using namespace std;

bool relatie1(TKey cheie1, TKey cheie2) {
  if (cheie1 <= cheie2) {
    return true;
  }
  else {
    return false;
  }
}

void testIteratorRemove() {
  std::cout << "Testing iterator remove." << std::endl;

  SortedMap m{relatie1};

  auto invalid = m.iterator();
  assert(!invalid.valid());

  std::cout << "Created" << std::endl;

  try {
    invalid.remove();
    assert(false);
  } catch (std::runtime_error&) {
    assert(true);
  }

  for (int i = 0; i < 10; ++i)
    m.add(i, i);

  auto it = m.iterator();
  it.next();
  it.next();
  it.next();

  assert(it.getCurrent().first == 3);
  it.remove();
  assert(it.getCurrent().first == 4);

  while (it.getCurrent().first < 9)
    it.next();

  it.remove();
  assert(!it.valid());
}

void testAll(){
  testIteratorRemove();

  SortedMap sm(relatie1);
  assert(sm.size() == 0);
  assert(sm.isEmpty());
    sm.add(1,2);
    assert(sm.size() == 1);
    assert(!sm.isEmpty());
    assert(sm.search(1)!=NULL_TVALUE);
    TValue v =sm.add(1,3);
    assert(v == 2);
    assert(sm.search(1) == 3);
    SMIterator it = sm.iterator();
    it.first();
    while (it.valid()){
      TElem e = it.getCurrent();
      assert(e.second != NULL_TVALUE);
      it.next();
    }
    assert(sm.remove(1) == 3);
    assert(sm.isEmpty());
}

