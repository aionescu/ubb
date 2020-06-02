#include <algorithm>
#include <stdexcept>
#include "SMIterator.h"
#include "SortedMap.h"

void swap(Node** a, Node** b) {
  auto t = *a;
  *a = *b;
  *b = t;
}
  
int partition(Node* arr[], int low, int high, Relation lte) {
  int pivot = arr[high]->kvp.first;
  int i = (low - 1);

  for (int j = low; j <= high - 1; j++) {
    if (lte(arr[j]->kvp.first, pivot)) {  
      i++;
      swap(&arr[i], &arr[j]);  
    }
  }

  swap(&arr[i + 1], &arr[high]);
  return (i + 1);
}
  
void quickSort(Node* arr[], int low, int high, Relation lte) {
  if (low < high) {
    int pi = partition(arr, low, high, lte);

    quickSort(arr, low, pi - 1, lte);
    quickSort(arr, pi + 1, high, lte);
  }
}

SMIterator::SMIterator(const SortedMap& map) : _map{map} {
  _lists = new Node*[_map._ht.m];
  _listCount = 0;

  for (std::size_t i = 0; i < _map._ht.m; ++i) {
    auto node = _map._ht.array[i];

    if (node)
      _lists[_listCount++] = node; 
  }

  quickSort(_lists, 0, _listCount - 1, _map._lte);

  _crrList = 0;
  _crrNode = _listCount ? _lists[_crrList] : nullptr;
}

SMIterator::~SMIterator() {
  delete[] _lists;
}

void SMIterator::first() {
  _crrList = 0;
  _crrNode = _lists[_crrList];
}

void SMIterator::next() {
  if (!valid())
    throw std::runtime_error{"SMIterator::getCurrent: Invalid iterator."};

  _crrNode = _crrNode->next;

  if (!_crrNode && _crrList < _listCount - 1) {
    ++_crrList;
    _crrNode = _lists[_crrList];
  }
}

bool SMIterator::valid() const {
  return _crrList < _listCount && _crrNode;
}

TElem SMIterator::getCurrent() const {
  if (!valid())
    throw std::runtime_error{"SMIterator::getCurrent: Invalid iterator."};

  return _crrNode->kvp;
}
