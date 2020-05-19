#include <cmath>
#include <exception>
#include <stdexcept>
#include "SMIterator.h"
#include "SortedMap.h"

std::size_t defaultHash(TKey key) {
  return (std::size_t)key;
}

SortedMap::SortedMap(Relation lt)
  : _lt{lt}
  , _n{0}
  , _ht{defaultHash, 8, new Node*[8]{}}
{ }

void deleteHT(HT& ht) {
  for (std::size_t i = 0; i < ht.m; ++i) {
    auto node = ht.array[i];

    while (node) {
      auto old = node;
      node = node->next;
      delete old;
    }
  }

  delete[] ht.array;
  ht = {};
}

void SortedMap::_forEach(std::function<void(TElem)> f) const {
  for (std::size_t i = 0; i < _ht.m; ++i) {
    auto node = _ht.array[i];

    while (node) {
      f(node->kvp);
      node = node->next;
    }
  }
}

Node** SortedMap::_getSlot(TKey key) const {
  auto index = _ht.hash(key) % _ht.m;
  return &_ht.array[index];
}

void SortedMap::_resize() {
  auto oldHT = _ht;
  _ht.m *= 2;
  _ht.array = new Node*[_ht.m]{};
  _n = 0;

  for (std::size_t i = 0; i < oldHT.m; ++i) {
    auto node = oldHT.array[i];

    while (node) {
      add(node->kvp.first, node->kvp.second);
      node = node->next;
    }
  }

  deleteHT(oldHT);
}

TValue SortedMap::add(TKey k, TValue v) {
  if ((double)_n / (double)_ht.m >= 1)
    _resize();

  auto slot = _getSlot(k);
  
  while (*slot && (*slot)->kvp.first != k)
    slot = &((*slot)->next);

  if (*slot) {
    auto old = (*slot)->kvp.second;
    (*slot)->kvp.second = v;

    return old;
  } else {
    *slot = new Node{{k, v}, nullptr};
    ++_n;

    return NULL_TVALUE;
  }
}

TValue SortedMap::search(TKey k) const {
  auto slot = _getSlot(k);

  while (*slot && (*slot)->kvp.first != k)
    slot = &((*slot)->next);

  return *slot ? (*slot)->kvp.second : NULL_TVALUE;
}

TValue SortedMap::remove(TKey k) {
  auto slot = _getSlot(k);

  if (!*slot)
    return NULL_TVALUE;
  
  if ((*slot)->kvp.first == k) {
    auto v = (*slot)->kvp.second;
    auto old = *slot;

    *slot = (*slot)->next;

    delete old;

    --_n;
    return v;
  }

  auto next = &((*slot)->next);

  while (*next && (*next)->kvp.first != k) {
    slot = next;
    next = &((*slot)->next);
  }

  if (*next) {
    (*slot)->next = (*next)->next;
    auto v = (*next)->kvp.second;

    delete *next;
    --_n;
    return v;
  }

  return NULL_TVALUE;
}

int SortedMap::size() const {
  return (int)_n;
}

bool SortedMap::isEmpty() const {
  return _n == 0;
}

SMIterator SortedMap::iterator() const {
  return SMIterator{*this};
}

SortedMap::~SortedMap() {
  deleteHT(_ht);
}
