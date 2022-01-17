#pragma once
//DO NOT INCLUDE SORTEDMAPITERATOR

#include <functional>
#include <utility>

//DO NOT CHANGE THIS PART
typedef int TKey;
typedef int TValue;
typedef std::pair<TKey, TValue> TElem;

#define NULL_TVALUE -111111
#define NULL_TPAIR std::pair<TKey, TValue>(-111111, -111111)

class SMIterator;
typedef bool (*Relation)(TKey, TKey);
typedef int Index;

struct Node {
  TElem kvp;
  Index left, right;
};

class SortedMap {
  friend class SMIterator;

  int _capacity, _size;
  Index _firstEmpty, _root;
  Node* _array;
  Relation _lte;

  void _resize();
  
  Index _allocate();
  void _deallocate(Index node);

  bool _canInsertAfter(Index node, TKey key);
  Index _findParentOfMaximum(Index original);

  void _inOrderTraversal(Index node, std::function<void(TElem)> f) const;

public:
  // implicit constructor
  SortedMap(Relation r);

  // Best case: Th(1)
  // Worst case: Th(h)
  // Avg case: O(h)
  // adds a pair (key,value) to the map
  //if the key already exists in the map, then the value associated to the key is replaced by the new value and the old value is returned
  //if the key SMes not exist, a new pair is added and the value null is returned
  TValue add(TKey c, TValue v);

  // Best case: Th(1)
  // Worst case: Th(h)
  // Avg case: O(h)
  //searches for the key and returns the value associated with the key if the map contains the key or null: NULL_TVALUE otherwise
  TValue search(TKey c) const;

  // Best case: Th(1)
  // Worst case: Th(h)
  // Avg case: O(h)
  //removes a key from the map and returns the value associated with the key if the key existed ot null: NULL_TVALUE otherwise
  TValue remove(TKey c);

  // Th(1)
  //returns the number of pairs (key,value) from the map
  int size() const;

  // Th(1)
  //checks whether the map is empty or not
  bool isEmpty() const;

  // Th(1)
  // return the iterator for the map
  // the iterator will return the keys following the order given by the relation
  SMIterator iterator();

  // destructor
  ~SortedMap();
};
