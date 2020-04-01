#pragma once

#include "Set.h"

class SetIterator
{
	friend class Set;
private:
	const Set& set;
	int idx;

	SetIterator(const Set& s);

public:
	void first();
	void next();

  // Best case: Theta(1)
  // Worst case: Theta(n)
  // Average case: O(n)
  void previous();
	TElem getCurrent();
	bool valid() const;
};
