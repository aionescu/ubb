#include "ShortTest.h"
#include <assert.h>
#include "Set.h"
#include "SetIterator.h"
#include <iostream>

void testPrevious() {
  std::cout << "Test previous" << std::endl;

  Set s;

  for (int i = 0; i < 10; ++i) {
    s.add(i);
    s.add(i * 2);
  }

  auto it = s.iterator();

  it.previous();
  assert(!it.valid());

  it.next();

  for (int i = 0; i < 10; ++i)
    it.next();

  assert(it.getCurrent() == 10);

  while (it.valid())
    it.next();

  it.previous();

  while (it.valid()) {
    std::cout << it.getCurrent() << ' ' << std::flush;
    it.previous();
  }

  std::cout << std::endl;
}

void testAll() { 
	Set s;
	assert(s.isEmpty() == true);
	assert(s.size() == 0); 
	assert(s.add(5)==true);
	assert(s.add(1)==true);
	assert(s.add(10)==true);
	assert(s.add(7)==true);
	assert(s.add(1)==false);
	assert(s.add(10)==false);
	assert(s.add(-3)==true);
	assert(s.size() == 5);
	assert(s.search(10) == true);
	assert(s.search(16) == false);
	assert(s.remove(1) == true);
	assert(s.remove(6) == false);
	assert(s.size() == 4);


	SetIterator it = s.iterator();
	it.first();
	int sum = 0;
	while (it.valid()) {
		TElem e = it.getCurrent();
		sum += e;
		it.next();
	}
	assert(sum == 19);

  testPrevious();
}