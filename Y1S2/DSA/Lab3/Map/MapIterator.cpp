#include <exception>
#include <stdexcept>
#include "Map.h"
#include "MapIterator.h"
using namespace std;

MapIterator::MapIterator(const Map& m) : _map{m}
{
	_crr = _map._head;
}

void MapIterator::first() {
  _crr = _map._head;
}

void MapIterator::next() {
  if (!valid())
    throw std::runtime_error{"MapIterator::next: Invalid iterator."};

  _crr = _map._array[_crr].next;
}

TElem MapIterator::getCurrent() {
  if (!valid())
    throw std::runtime_error{"MapIterator::getCurrent: Invalid iterator."};
	
  return _map._array[_crr].value;
}

bool MapIterator::valid() const {
	return _crr != -1;
}

void MapIterator::jumpBackward(int k) {
  if (k <= 0)
    throw std::out_of_range{"MapIterator::jumpBackward: Number of steps must be strictly positive."};

  if (_crr == -1)
    throw std::runtime_error{"MapIterator::jumpBackward: Invalid iterator."};

  while (k-- && _crr != -1)
    _crr = _map._array[_crr].prev;
}