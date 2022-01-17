#pragma once
#include "SortedMap.h"

//DO NOT CHANGE THIS PART
class SMIterator{
  friend class SortedMap;
private:
  // Because we can't store the elements in an ordered way inside
  // the hashtable, when constructing the iterator we copy all of the
  // elements into an array, then sort the array.
  SortedMap& map;
  TElem* elems;
  std::size_t currentIndex;

  SMIterator(SortedMap& mapionar);

public:
  void first();
  void next();
  bool valid() const;
  TElem getCurrent() const;

  // Best case: Th(1)
  // Worst case: Th(n)
  // Average case: O(n)
  TElem remove();
};
