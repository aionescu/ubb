#include <stdexcept>
#include "SetIterator.h"
#include "Set.h"

SetIterator::SetIterator(const Set& s) : set{s} {
	first();
}

void SetIterator::first() {
	idx = -1;
	next();
}

void SetIterator::next() {
	if (set.capacity == 0)
		return;

	do {
		++idx;
	} while (idx <= set.maxIdx && !set.array[idx]);
}

TElem SetIterator::getCurrent() {
	if (!valid())
	  throw std::out_of_range{"SetIterator::getCurrent: Invalid iterator"};

	return idx + set.minElem;
}

bool SetIterator::valid() const {
	return set.capacity > 0 && idx >= 0 && idx <= set.maxIdx;
}