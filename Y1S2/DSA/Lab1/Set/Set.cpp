#include <stdexcept>
#include "Set.h"
#include "SetIterator.h"

Set::Set() : capacity{0}, count{0}, array{nullptr}, minElem{} { }

bool Set::add(TElem elem) {
	if (capacity == 0) {
		capacity = 64;
		count = 1;

		array = new TElem[64]{};
		array[0] = true;

		minElem = elem;
		return true;
	}

	auto idx = elem - minElem;

	if (idx >= 0 && idx < capacity) {
		if (array[idx])
		  return false;

		array[idx] = true;
		++count;

		return true;
	} else if (idx < 0) {
		auto diff = -idx;
		auto newCapacity = capacity + 64;

		while (newCapacity < capacity + diff)
		  newCapacity += 64;

		auto newArray = new TElem[newCapacity]{};

		for (int i = 0; i < capacity; ++i) {
			newArray[i + diff] = array[i];
		}

		newArray[0] = true;

		delete[] array;
		array = newArray;

		capacity = newCapacity;
		++count;
		minElem = elem;

		return true;
	} else if (idx >= capacity) {
		auto diff = idx - capacity;
		auto newCapacity = capacity + 64;

		while (newCapacity < capacity + diff)
		  newCapacity += 64;

		auto newArray = new TElem[newCapacity]{};

		for (int i = 0; i < capacity; ++i) {
			newArray[i] = array[i];
		}

		newArray[idx] = true;

		delete[] array;
		array = newArray;

		capacity = newCapacity;
		++count;

		return true;
	} else {
		throw std::logic_error{"Set::add: Unreachable"};
	}
}

bool Set::remove(TElem elem) {
	auto idx = elem - minElem;

	if (capacity == 0 || idx < 0 || idx >= capacity || !array[idx])
		return false;

	array[idx] = false;
	--count;

	return true;
}

bool Set::search(TElem elem) const {
	auto idx = elem - minElem;
	return idx >= 0 && idx < capacity && array[idx];
}

int Set::size() const {
	return count;
}

bool Set::isEmpty() const {
	return count == 0;
}

Set::~Set() {
	capacity = count = 0;
	minElem.~TElem();

	if (array) {
		delete[] array;
		array = nullptr;
	}
}

SetIterator Set::iterator() const {
	return SetIterator(*this);
}