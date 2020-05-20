#include <algorithm>
#include <cassert>
#include <exception>
#include <iostream>
#include "SortedMap.h"
#include "SMIterator.h"
#include "ShortTest.h"

using namespace std;

bool relatie1(TKey cheie1, TKey cheie2) {
  if (cheie1 <= cheie2) {
    return true;
  }
  else {
    return false;
  }
}

void testValueBag() {
  std::cout << "Testing value bag." << std::endl;

  SortedMap map{relatie1};

  for (int i = 0; i < 10; ++i)
    map.add(i, i * 2);

  auto vec = map.valueBag();
  std::sort(vec.begin(), vec.end());
  
  assert(vec.size() == 10);

  std::vector<TValue> expected{{0, 2, 4, 6, 8, 10, 12, 14, 16, 18}};
  assert(vec == expected);

  SortedMap empty{relatie1};

  auto emptyVec = empty.valueBag();
  assert(emptyVec.size() == 0);
}

void testAll(){
  testValueBag();

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
