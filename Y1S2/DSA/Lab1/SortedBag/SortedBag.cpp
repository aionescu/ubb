#include <algorithm>
#include "SortedBag.h"
#include "SortedBagIterator.h"

SortedBag::SortedBag(Relation r) {
	length = 0;
	capacity = 0;
	relation = r;
	array = nullptr;
}

void SortedBag::add(TComp e) {
	for (int i = 0; i < length; ++i)
		if (array[i].first == e) {
			array[i].second++;
			return;
		}

	if (capacity == 0) {
		capacity = 1;
		length = 1;

		array = new std::pair<TComp, int>[1];
		array[0] = {e, 1};

		return;
	}

	if (length < capacity) {
		array[length++] = {e, 1};
	} else {
		int newCapacity = capacity * 2;
		auto newArray = new std::pair<TComp, int>[newCapacity];

		for (int i = 0; i < length; ++i)
			newArray[i] = array[i];

		delete[] array;
		array = newArray;

		capacity = newCapacity;
		array[length++] = {e, 1};
	}

	auto lambda = [&](std::pair<TComp, int> a, std::pair<TComp, int> b) { return relation(a.first, b.first); };
	std::sort(array, array + length, lambda);
}

bool SortedBag::remove(TComp e) {
	for (int i = 0; i < length; ++i)
		if (array[i].first == e) {
			array[i].second--;

			if (array[i].second == 0) {
				for (int j = i; j < length - 1; ++j)
					array[j] = array[j + 1];

				length--;
			}

			return true;
		}

	return false;
}

bool SortedBag::search(TComp elem) const {
	for (int i = 0; i < length; ++i)
		if (array[i].first == elem)
			return true;

	return false;
}


int SortedBag::nrOccurrences(TComp elem) const {
	for (int i = 0; i < length; ++i)
		if (array[i].first == elem)
			return array[i].second;

	return 0;
}

int SortedBag::size() const {
	int sum = 0;

	for (int i = 0; i < length; ++i)
		sum += array[i].second;

	return sum;
}

bool SortedBag::isEmpty() const {
	return length == 0;
}


SortedBagIterator SortedBag::iterator() const {
	// py: return SortedBagIterator(self)
	return SortedBagIterator(*this);
}


SortedBag::~SortedBag() {
	length = 0;
	capacity = 0;
	relation = nullptr;

	if (array != nullptr) {
		delete[] array;
		array = nullptr;
	}
}