#include "Set.h"
#include "SetIterator.h"
#include <iostream>

Set::Set() {
	_size = _elemCount = 0;
	_min = _max = 0;
	_array = nullptr; // No allocations if there are no elements
}

int evil_global = 0;

bool Set::add(TElem elem) {
	if (_size == 0) {
		_array = new TElem[1];
		_size = _elemCount = 1;
		_min = _max = elem;

		_array[0] = true;
		return true;
	}

	if (elem >= _min && elem <= _max) {
		auto idx = elem - _min;
		if (_array[idx])
			return false;

		_array[idx] = true;
		++_elemCount;
		return true;
	} else if (elem < _min) {
		auto diff = _min - elem;
		auto newSize = _size + diff;
		auto newArray = new TElem[newSize];

		for (auto i = 0; i < _size; ++i) {
			newArray[i + diff] = _array[i];
		}

		newArray[0] = true; // Set new value

		delete[] _array;

		_size = newSize;
		++_elemCount;
		_min = elem;
		_array = newArray;

		return true;
	} else { // elem > _max
		auto diff = elem - _max;
		auto newSize = _size + diff;
		auto newArray = new TElem[newSize];

		for (auto i = 0; i < _size; ++i) {
			newArray[i] = _array[i];
		}

		newArray[newSize - 1] = true; // Set new value

		delete[] _array;

		_size = newSize;
		++_elemCount;
		_max = elem;
		_array = newArray;

		return true;
	}
}


bool Set::remove(TElem elem) {
	if (_size == 0)
		return false;

	auto idx = elem - _min;

	if (elem < _min || elem > _max || !_array[idx])
		return false;

	_array[idx] = false;
	--_elemCount;
	return true;
}

bool Set::search(TElem elem) const {
	auto idx = elem - _min;
	return idx >= 0 && idx < _size && _array[idx];
}


int Set::size() const {
	return _elemCount;
}


bool Set::isEmpty() const {
	return _elemCount == 0;
}


Set::~Set() {
	_size = _elemCount = 0;
	_min = _max = 0;
	delete[] _array;
}


SetIterator Set::iterator() const {
	return SetIterator(*this);
}