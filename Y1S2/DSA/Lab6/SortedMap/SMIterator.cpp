#include <exception>
#include <stdexcept>
#include "SMIterator.h"
#include "SortedMap.h"

SMIterator::SMIterator(const SortedMap& m) : _map{m} {
  if (_map._size == 0) {
    _current = 0;
    _elems = nullptr;

    return;
  }

  _elems = new TElem[_map._size];
  _current = 0;

  _map._inOrderTraversal(_map._root, [&](TElem elem) { _elems[_current++] = elem; });
  _current = 0;
}

SMIterator::~SMIterator() {
  if (_elems)
    delete[] _elems;
}

void SMIterator::first() {
  _current = 0;
}

void SMIterator::next() {
  if (!valid())
    throw std::runtime_error{"SMIterator::next: Invalid iterator."};

  ++_current;
}

bool SMIterator::valid() const {
  return _current < _map._size;
}

TElem SMIterator::getCurrent() const {
  if (!valid())
    throw std::runtime_error{"SMIterator::getCurrent: Invalid iterator."};

  return _elems[_current];
}
