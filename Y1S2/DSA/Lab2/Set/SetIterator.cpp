#include <exception>
#include "SetIterator.h"
#include "Set.h"

class InvalidIterator : public std::exception {};

SetIterator::SetIterator(const Set &m) : set(m) {
  _crr = set._list.head();
  _idx = 0;
}

void SetIterator::first() {
  _crr = set._list.head();
  _idx = 0;
}

void SetIterator::next() {
  if (!_crr)
    throw InvalidIterator{};

  _crr = _crr->next;
  ++_idx;
}

void SetIterator::previous() {
  --_idx;
  _crr = set._list.nodeAt(_idx);
}

TElem SetIterator::getCurrent() {
  if (!_crr)
    throw InvalidIterator();

  return _crr->value;
}

bool SetIterator::valid() const {
  return _crr != nullptr;
}