#include "SetIterator.h"
#include "Set.h"


SetIterator::SetIterator(const Set& m) : _set(m) {
	first();
}

void SetIterator::first() {
	_idx = -1;

	next();
}

void SetIterator::next() {
	++_idx;

	while (_idx < _set._size && !_set._array[_idx])
		++_idx;
}

TElem SetIterator::getCurrent() {
	return _idx + _set._min;
}

bool SetIterator::valid() const {
	return _idx < _set._size;
}