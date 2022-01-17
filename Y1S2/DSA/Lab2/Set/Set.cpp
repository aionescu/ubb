#include "Set.h"
#include "SetIterator.h"

bool Set::add(TElem elem) {
  if (_list.contains(elem))
    return false;

  _list.add(elem);
  return true;
}

bool Set::remove(TElem elem) {
  return _list.remove(elem);
}

bool Set::search(TElem elem) const {
  return _list.contains(elem);
}

int Set::size() const {
  return _list.length();
}

bool Set::isEmpty() const {
  return _list.length() == 0;
}

SetIterator Set::iterator() const {
  return SetIterator(*this);
}