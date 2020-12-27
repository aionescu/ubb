#include "SortedBagIterator.h"
#include "SortedBag.h"
#include <exception>
#include <stdexcept>

using namespace std;

SortedBagIterator::SortedBagIterator(const SortedBag& b) : bag(b) {
	currentIndex = 0;
	currentFreq = 1;
}

TComp SortedBagIterator::getCurrent() {
	if (!valid())
		throw std::runtime_error{"Invalid iterator."};

	return bag.array[currentIndex].first;
}

bool SortedBagIterator::valid() {
	return currentIndex < bag.length;
}

void SortedBagIterator::next() {
	if (!valid())
		throw std::runtime_error{"Invalid iterator."};

	if (currentFreq >= bag.array[currentIndex].second) {
		currentIndex++;
		currentFreq = 1;
	} else {
		currentFreq++;
	}
}

void SortedBagIterator::first() {
	currentIndex = 0;
	currentFreq = 1;
}