#pragma once

#include <stack>
#include "SortedMap.h"

//DO NOT CHANGE THIS PART
class SMIterator{
  friend class SortedMap;

  SortedMap& _map;
  TElem* _elems;
  int _current;

  SMIterator(SortedMap& mapionar);

public:
  void first();
  void next();
  bool valid() const;
  TElem getCurrent() const;

  // Best case: Th(1)
  // Worst case: Th(h + n)
  // Avg case: O(h + n)
  TElem remove();

  ~SMIterator();
};
