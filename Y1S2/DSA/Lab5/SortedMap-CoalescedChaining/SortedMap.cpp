#include <exception>
#include "SMIterator.h"
#include "SortedMap.h"

std::size_t defaultHash(TKey key) {
  return (std::size_t)key;
}

SortedMap::SortedMap(Relation r) {
  this->rel = r;
  this->hashFn = defaultHash;

  this->m = 8;
  this->pairs = new TElem[8];
  this->next = new int[8];

  // Initialize all elements in this->pairs to be empty
  // Initialize all elements in this-next to be -1
  for (std::size_t i = 0; i < 8; ++i) {
    this->pairs[i] = NULL_TPAIR;
    this->next[i] = -1;
  }

  this->firstEmpty = 0;
  this->n = 0;
}

std::size_t SortedMap::getIndex(TKey key) const {
  return this->hashFn(key) % this->m;
}

void SortedMap::resize() {
  // Copy old hashtable data to some variables
  auto oldM = this->m;
  auto oldPairs = this->pairs;
  auto oldNext = this->next;

  // Update hashtable
  this->m *= 2;
  this->n = 0;
  this->firstEmpty = 0;
  this->pairs = new TElem[this->m];
  this->next = new int[this->m];

  // Initialize all elements in this->pairs to be empty
  // Initialize all elements in this-next to be -1
  for (std::size_t i = 0; i < this->m; ++i) {
    this->pairs[i] = NULL_TPAIR;
    this->next[i] = -1;
  }

  // For each element in the old hashtable, add it to the new hashtable
  for (std::size_t i = 0; i < oldM; ++i)
    if (oldPairs[i] != NULL_TPAIR)
      this->add(oldPairs[i].first, oldPairs[i].second);

  delete[] oldPairs;
  delete[] oldNext;
}

void SortedMap::changeFirstEmpty() {
  for (std::size_t i = 0; i < this->m; ++i)
    if (this->pairs[i] == NULL_TPAIR) {
      this->firstEmpty = i;
      return;
    }

  this->firstEmpty = -1;
}

TValue SortedMap::add(TKey k, TValue v) {
  if (this->m == this->n)
    this->resize();

  std::size_t index = this->getIndex(k);

  // If no linked list exists at the index
  if (this->pairs[index] == NULL_TPAIR) {
    // this->pairs[index] = {k, v};
    this->pairs[index].first = k;
    this->pairs[index].second = v;
    this->next[index] = -1;

    if (index == (std::size_t)this->firstEmpty)
      this->changeFirstEmpty();

    this->n++;
    return NULL_TVALUE;
  }

  // If the first node in the list is the element we are looking for
  if (this->pairs[index].first == k) {
    TValue oldValue = this->pairs[index].second;
    this->pairs[index].second = v;

    return oldValue;
  }

  std::size_t currentIndex = index;

  // Parse the linked list to find the element we are looking for
  while (this->next[currentIndex] != -1 && this->pairs[currentIndex].first != k)
    currentIndex = this->next[currentIndex];

  // If we didn't find any element with the same key
  if (this->next[currentIndex] == -1) {
    this->pairs[this->firstEmpty].first = k;
    this->pairs[this->firstEmpty].second = v;
    this->next[this->firstEmpty] = -1;

    this->next[currentIndex] = this->firstEmpty;
    this->changeFirstEmpty();

    this->n++;
    return NULL_TVALUE;
  } else { // If we found an element with the same key
    TValue oldValue = this->pairs[currentIndex].second;
    this->pairs[currentIndex].second = v;

    return oldValue;
  }
}

TValue SortedMap::search(TKey k) const {
  std::size_t index = this->getIndex(k);

  // If there is no linked list at the index
  if (this->pairs[index] == NULL_TPAIR)
    return NULL_TVALUE;

  // If the first node in the list is the element we are looking for
  if (this->pairs[index].first == k)
    return this->pairs[index].second;

  // Parse the linked list to find the element we are looking for
  while (this->next[index] != -1 && this->pairs[index].first != k)
    index = this->next[index];

  // If we didn't find any element with the same key
  if (this->next[index] == -1)
    return NULL_TVALUE;
  else // If we found an element with the key we are searching for
    return this->pairs[index].second;
}

TValue SortedMap::remove(TKey k) {
  std::size_t index = this->getIndex(k);

  // If there is no linked list at the index
  if (this->pairs[index] == NULL_TPAIR)
    return NULL_TVALUE;

  // If the first node in the list is the element we are looking for
  if (this->pairs[index].first == k) {
    TValue oldValue = this->pairs[index].second;

    auto next = this->next[index];

    // If the node we found is the only node in the list.
    if (next == -1) {
      this->pairs[index] = NULL_TPAIR;
    } else { // Otherwise, the node is followed by at least 1 other node, and we need to preserve that node.
      this->pairs[index] = this->pairs[next];
      this->next[index] = this->next[next];

      this->pairs[next] = NULL_TPAIR;
      this->next[next] = -1;
    }

    this->n--;
    this->changeFirstEmpty();
    return oldValue;
  }

  // If there is only one element in the list, and it is NOT the element
  // we are looking for, then we just return NULL_TVALUE.
  if (this->next[index] == -1)
    return NULL_TVALUE;

  std::size_t prev = index;
  index = this->next[index];

  // Parse the linked list to find the element we are looking for
  while (this->next[index] != -1 && this->pairs[index].first != k) {
    prev = index;
    index = this->next[index];
  }

  // If we didn't find any element with the same key
  if (this->next[index] == -1)
    return NULL_TVALUE;
  else { // If we found an element with the key we are searching for
    TValue oldValue = this->pairs[index].second;

    this->next[prev] = this->next[index];
    this->pairs[index] = NULL_TPAIR;
    this->next[index] = -1;

    this->n--;
    this->changeFirstEmpty();
    return oldValue;
  }
}

int SortedMap::size() const {
  return (int)n;
}

bool SortedMap::isEmpty() const {
  return n == 0;
}

SMIterator SortedMap::iterator() {
  return SMIterator(*this);
}

SortedMap::~SortedMap() {
  delete[] this->pairs;
  delete[] this->next;
}
