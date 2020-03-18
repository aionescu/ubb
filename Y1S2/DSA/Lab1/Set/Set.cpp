#include "Set.h"
#include "SetIterator.h"

Set::Set() : capacity{0}, count{0}, maxIdx{0}, array{nullptr}, minElem{} { }

bool Set::add(TElem elem) {
	if (capacity == 0) {
		capacity = 1;
		count = 1;
		maxIdx = 0;

		array = new TElem[1]{};
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

		if (idx > maxIdx)
			maxIdx = idx;

		return true;
	} else if (idx < 0) {
		auto diff = -idx;

		if (maxIdx + diff < capacity) {
			for (auto i = maxIdx; i >= 0; --i) {
				array[i + diff] = array[i];
				array[i] = false;
			}

			array[0] = true;
			++count;

			maxIdx += diff;
			minElem = elem;

			return true;
		}

		auto newCapacity = capacity * 2;

		while (newCapacity < capacity + diff)
		  newCapacity *= 2;

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
		maxIdx += diff;

		return true;
	} else { // if (idx >= capacity)
		auto diff = idx - maxIdx;
		auto newCapacity = capacity * 2;

		while (newCapacity < capacity + diff)
		  newCapacity *= 2;

		auto newArray = new TElem[newCapacity]{};

		for (int i = 0; i < capacity; ++i) {
			newArray[i] = array[i];
		}

		newArray[idx] = true;

		delete[] array;
		array = newArray;

		capacity = newCapacity;
		++count;
		maxIdx = idx;

		return true;
	}
}

bool Set::remove(TElem elem) {
	auto idx = elem - minElem;

	if (capacity == 0 || idx < 0 || idx > maxIdx || !array[idx])
		return false;

	array[idx] = false;
	--count;

	return true;
}

bool Set::search(TElem elem) const {
	auto idx = elem - minElem;
	return capacity > 0 && idx >= 0 && idx <= maxIdx && array[idx];
}

int Set::size() const {
	return count;
}

bool Set::isEmpty() const {
	return count == 0;
}

Set::~Set() {
	capacity = count = maxIdx = 0;
	minElem.~TElem();

	if (array) {
		delete[] array;
		array = nullptr;
	}
}

SetIterator Set::iterator() const {
	return SetIterator(*this);
}