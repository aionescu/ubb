#include <exception>
#include "SMIterator.h"
#include "SortedMap.h"

SortedMap::SortedMap(Relation r)
  : _capacity{8}
  , _size{0}
  , _firstEmpty{0}
  , _root{-1}
  , _array{new Node[8]}
  , _lte{r}
{
  for (auto i = 0; i < _capacity; ++i)
    _array[i] = {NULL_TPAIR, -1, i + 1};

  _array[_capacity - 1].right = -1;
}

void SortedMap::_resize() {
  auto oldArray = _array;
  auto oldCapacity = _capacity;

  _capacity *= 2;
  _array = new Node[_capacity];

  for (auto i = 0; i < oldCapacity; ++i)
    _array[i] = oldArray[i];

  for (auto i = oldCapacity; i < _capacity; ++i)
    _array[i] = {NULL_TPAIR, -1, i + 1};

  _array[_capacity - 1].right = -1;
  _firstEmpty = oldCapacity;

  delete[] oldArray;
}

Index SortedMap::_allocate() {
  if (_firstEmpty == -1)
    _resize();

  auto node = _firstEmpty;
  _firstEmpty = _array[_firstEmpty].right;

  _array[node] = {NULL_TPAIR, -1, -1};

  ++_size;
  return node;
}

void SortedMap::_deallocate(Index node) {
  _array[node] = {NULL_TPAIR, -1, _firstEmpty};
  _firstEmpty = node;
  --_size;
}

bool SortedMap::_canInsertAfter(Index node, TKey key) {
  return
    _array[node].kvp.first == key
    || (_lte(_array[node].kvp.first, key) && _array[node].right == -1)
    || (!_lte(_array[node].kvp.first, key) && _array[node].left == -1);
}

void SortedMap::_inOrderTraversal(Index node, std::function<void(TElem)> f) const {
  if (node == -1)
    return;

  _inOrderTraversal(_array[node].left, f);
  f(_array[node].kvp);
  _inOrderTraversal(_array[node].right, f);
}

Index SortedMap::_findParentOfMaximum(Index original) {
  if (original == -1 || _array[original].left == -1)
    return -1;

  auto startingNode = _array[original].left;

  if (startingNode == -1)
    return -1;
  
  if (_array[startingNode].left == -1 && _array[startingNode].right == -1)
    return original;

  if (startingNode == -1 || _array[startingNode].right == -1)
    return -1;

  auto parent = startingNode;
  auto prev = _array[startingNode].right;
  auto node = _array[prev].right;

  while (node != -1) {
    parent = prev;
    prev = node;
    node = _array[node].right;
  }

  return parent;
}

TValue SortedMap::add(TKey k, TValue v) {
  if (_root == -1) {
    _root = _allocate();
    _array[_root].kvp = {k, v};

    return NULL_TVALUE;
  }

  auto node = _root;

  while (!_canInsertAfter(node, k)) {
    if (_lte(_array[node].kvp.first, k))
      node = _array[node].right;
    else
      node = _array[node].left;
  }

  if (_array[node].kvp.first == k) {
    auto oldV = _array[node].kvp.second;
    _array[node].kvp.second = v;

    return oldV;
  } else {
    auto newNode = _allocate();
    _array[newNode].kvp = {k, v};

    if (_lte(_array[node].kvp.first, k))
      _array[node].right = newNode;
    else
      _array[node].left = newNode;

    return NULL_TVALUE;
  }
}

TValue SortedMap::search(TKey k) const {
  auto node = _root;

  while (node != -1 && _array[node].kvp.first != k)
    if (_lte(_array[node].kvp.first, k))
      node = _array[node].right;
    else
      node = _array[node].left;

  return node == -1 ? NULL_TVALUE : _array[node].kvp.second;
}

TValue SortedMap::remove(TKey k) {
  if (_root == -1) 
    return NULL_TVALUE;

  if (_array[_root].kvp.first == k) {
    auto v = _array[_root].kvp.second;

    if (_array[_root].left == -1 && _array[_root].right == -1) {
      _deallocate(_root);
      _root = -1;
    } else if (_array[_root].left == -1) {
      auto oldRoot = _root;
      _root = _array[_root].right;
      _deallocate(oldRoot);
    } else if (_array[_root].right == -1) {
      auto oldRoot = _root;
      _root = _array[_root].left;
      _deallocate(oldRoot);
    } else {
      auto parentMax = _findParentOfMaximum(_root);

      Index max;
      if (parentMax != _root) {
        max = _array[parentMax].right;
        _array[parentMax].right = -1;
      } else {
        max = _array[parentMax].left;
        _array[parentMax].left = -1;
      }
      
      _array[_root].kvp = _array[max].kvp;
      _deallocate(max);
    }

    return v;
  }

  auto prev = _root;
  auto dir = _lte(_array[_root].kvp.first, k);
  auto node = dir ? _array[_root].right : _array[_root].left;

  while (node != -1 && _array[node].kvp.first != k) {
    prev = node;
    dir = _lte(_array[node].kvp.first, k);
    node = dir ? _array[node].right : _array[node].left;
  }

  if (node == -1)
    return NULL_TVALUE;

  auto v = _array[node].kvp.second;

  if (_array[node].left == -1 && _array[node].right == -1) {
    if (dir)
      _array[prev].right = -1;
    else
      _array[prev].left = -1;

    _deallocate(node);
  } else if (_array[node].left == -1) {
    if (dir)
      _array[prev].right = _array[node].right;
    else
      _array[prev].left = _array[node].right;

    _deallocate(node);
  } else if (_array[node].right == -1) {
    if (dir)
      _array[prev].right = _array[node].left;
    else
      _array[prev].left = _array[node].left;

    _deallocate(node);
  } else {
    auto parentMax = _findParentOfMaximum(node);
    
    Index max;
    if (parentMax != node) {
      max = _array[parentMax].right;
      _array[parentMax].right = -1;
    } else {
      max = _array[parentMax].left;
      _array[parentMax].left = -1;
    }

    _array[node].kvp = _array[max].kvp;
    _deallocate(max);
  }

  return v;
}

int SortedMap::size() const {
  return _size;
}

bool SortedMap::isEmpty() const {
  return _size == 0;
}

SMIterator SortedMap::iterator() {
  return SMIterator(*this);
}

SortedMap::~SortedMap() {
  delete[] _array;
}
