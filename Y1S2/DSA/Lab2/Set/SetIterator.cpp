#include <exception>
#include "SetIterator.h"
#include "Set.h"

class InvalidIterator : public std::exception {};

SetIterator::SetIterator(const Set &m) : set(m) {
  _crr = set._list.head();
}

void SetIterator::first() {
  _crr = set._list.head();
}

void SetIterator::next() {
  if (!_crr)
    throw InvalidIterator{};

  _crr = _crr->next;
}

TElem SetIterator::getCurrent() {
  if (!_crr)
    throw InvalidIterator();

  return _crr->value;
}

bool SetIterator::valid() const {
  return _crr != nullptr;
}