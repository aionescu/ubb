#include <algorithm>
#include <stdexcept>
#include "SMIterator.h"
#include "SortedMap.h"

SMIterator::SMIterator(const SortedMap& map) : _map{map}, _crr{0} {
  _kvps = new TElem[_map._n];
  
  auto idx = _crr;
  _map._forEach([&](TElem kvp) { _kvps[idx++] = kvp; });

  std::sort(_kvps, _kvps + _map._n, [&](const TElem& a, const TElem& b) { return _map._lt(a.first, b.first); });
}

SMIterator::~SMIterator() {
  delete[] _kvps;
}

void SMIterator::first() {
  _crr = 0;
}

void SMIterator::next() {
  if (!valid())
    throw std::runtime_error{"SMIterator::getCurrent: Invalid iterator."};

  ++_crr;
}

bool SMIterator::valid() const {
  return _crr < _map._n;
}

TElem SMIterator::getCurrent() const {
  if (!valid())
    throw std::runtime_error{"SMIterator::getCurrent: Invalid iterator."};

  return _kvps[_crr];
}
