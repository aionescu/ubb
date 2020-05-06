#include <cassert>
#include "Map.h"
#include "MapIterator.h"

bool Map::_needsToGrow() const {
  // assert((_length == _capacity) == (_firstEmpty == -1));
  return _length == _capacity;
}

void Map::_grow() {
  _capacity *= 2;

  auto newBuffer = new Node[_capacity];

  for (int i = 0; i < _length; ++i)
    newBuffer[i] = _array[i];

  for (int i = _length; i < _capacity; ++i)
    newBuffer[i] = {NULL_TELEM, -1, i + 1};

  newBuffer[_capacity - 1].next = -1;
  _firstEmpty = _length;

  delete[] _array;
  _array = newBuffer;
}

Index Map::_searchIdx(TKey c) const {
	for (auto crr = _head; crr != -1; crr = _array[crr].next)
    if (_array[crr].value.first == c)
      return crr;

  return -1;
}

Map::Map() {
  _capacity = 8;
  _array = new Node[8];

  for (int i = 0; i < 8; ++i)
    _array[i] = {NULL_TELEM, -1, i + 1};

  _array[7].next = -1;
  
  _length = 0;

  _head = _tail = -1;
  _firstEmpty = 0;
}

Map::~Map() {
  delete[] _array;
}

TValue Map::add(TKey c, TValue v) {
  auto oldIdx = _searchIdx(c);

	if (oldIdx != -1) {
    auto old = _array[oldIdx].value.second;
    _array[oldIdx].value.second = v;

    return old;
  }

  if (_needsToGrow())
    _grow();

  auto nextEmpty = _array[_firstEmpty].next;

  _array[_firstEmpty] = {{c, v}, _tail, -1};

  if (_tail != -1)
    _array[_tail].next = _firstEmpty;

  _tail = _firstEmpty;
  if (_head == -1)
    _head = _firstEmpty;

  _firstEmpty = nextEmpty;
  ++_length;

  return NULL_TVALUE;
}

TValue Map::search(TKey c) const {
  auto idx = _searchIdx(c);
  return idx != -1 ? _array[idx].value.second : NULL_TVALUE;
}

TValue Map::remove(TKey c) {
  auto idx = _searchIdx(c);

  if (idx == -1)
    return NULL_TVALUE;

  auto old = _array[idx].value.second;

  if (idx != _head)
    _array[_array[idx].prev].next = _array[idx].next;
  else
    _head = _array[idx].next;

  if (idx != _tail)
    _array[_array[idx].next].prev = _array[idx].prev;
  else
    _tail = _array[idx].prev;

  _array[idx] = {NULL_TELEM, -1, _firstEmpty};
  _firstEmpty = idx;
  --_length;

  return old;
}

int Map::size() const {
	return _length;
}

bool Map::isEmpty() const {
	return _length == 0;
}

MapIterator Map::iterator() const {
	return MapIterator(*this);
}