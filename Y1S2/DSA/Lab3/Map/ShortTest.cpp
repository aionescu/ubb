#include "ShortTest.h"
#include <assert.h>
#include "Map.h"
#include "MapIterator.h"
#include <iostream> //!

void testJumpBackward() {
  std::cout << "Test jumpBackward" << std::endl;

  Map m;
  
  for (int i = 0; i < 10; ++i)
    m.add(i, i * 10);

  auto it = m.iterator();

  it.first();

  for (int i = 0; i < 5; ++i)
    it.next();

  auto crr = it.getCurrent(); // {5, 50}

  try {
    it.jumpBackward(-1);
    assert(false);
  } catch (std::exception&) { }

  it.jumpBackward(1);
  crr = {4, 40};
  assert(it.getCurrent() == crr);

  it.jumpBackward(3);
  crr = {1, 10};
  assert(it.getCurrent() == crr);

  it.jumpBackward(2);
  assert(!it.valid());
}

void testAll() { //call each function to see if it is implemented
  testJumpBackward();

	Map m;
	assert(m.isEmpty() == true);
	assert(m.size() == 0); //add elements
	assert(m.add(5,5)==NULL_TVALUE);
	assert(m.add(1,111)==NULL_TVALUE);
	assert(m.add(10,110)==NULL_TVALUE);
	assert(m.add(7,7)==NULL_TVALUE);
	assert(m.add(1,1)==111);
	assert(m.add(10,10)==110);
	assert(m.add(-3,-3)==NULL_TVALUE);
	assert(m.size() == 5);
	assert(m.search(10) == 10);
	assert(m.search(16) == NULL_TVALUE);
	assert(m.remove(1) == 1);
	assert(m.remove(6) == NULL_TVALUE);
	assert(m.size() == 4);

	TElem e;
	MapIterator id = m.iterator();
	id.first();
	int s1 = 0, s2 = 0;
	while (id.valid()) {
		e = id.getCurrent();
		s1 += e.first;
		s2 += e.second;
		id.next();
	}
	assert(s1 == 19);
	assert(s2 == 19);

}


