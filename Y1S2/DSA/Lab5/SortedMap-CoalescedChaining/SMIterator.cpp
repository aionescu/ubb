#include <exception>
#include <stdexcept>
#include <algorithm>
#include "SMIterator.h"
#include "SortedMap.h"

void swap(TElem& a, TElem& b) {  
  auto t = a;  
  a = b;  
  b = t;  
}

int partition(TElem arr[], int low, int high, Relation r) {  
  TElem pivot = arr[high];
  int i = (low - 1);

  for (int j = low; j <= high - 1; j++) {
    if (r(arr[j].first, pivot.first)) { // <--- Using relation here
      i++;
      swap(arr[i], arr[j]);
    }
  }

  swap(arr[i + 1], arr[high]);
  return (i + 1);
}

void quickSort(TElem arr[], int low, int high, Relation r) {
  if (low < high) {
    int pi = partition(arr, low, high, r);

    quickSort(arr, low, pi - 1, r);
    quickSort(arr, pi + 1, high, r);
  }
}

SMIterator::SMIterator(const SortedMap& m) : map(m) {
  this->elems = new TElem[map.n];
  this->currentIndex = 0;

  // For each element in the hashtable, add it to the array
  // at the current index, then increment the index.
  for (std::size_t i = 0; i < map.m; ++i)
    if (map.pairs[i] != NULL_TPAIR)
      this->elems[this->currentIndex++] = map.pairs[i];

  this->currentIndex = 0;
  quickSort(this->elems, 0, this->map.n - 1, this->map.rel);
  // std::sort(this->elems, this->elems + this->map.n, [&](const TElem& a, const TElem& b) { return this->map.rel(a.first, b.first); });
}

void SMIterator::first() {
  this->currentIndex = 0;
}

void SMIterator::next() {
  if (!this->valid())
    throw std::runtime_error{"Invalid iterator in next() function."};

  this->currentIndex++;
}

bool SMIterator::valid() const {
  return this->currentIndex < this->map.n;
}

TElem SMIterator::getCurrent() const {
  if (!this->valid())
    throw std::runtime_error{"Invalid iterator in getCurrent() function."};

  return this->elems[this->currentIndex];
}
