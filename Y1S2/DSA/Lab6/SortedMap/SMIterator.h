#pragma once

#include <stack>
#include "SortedMap.h"

//DO NOT CHANGE THIS PART
class SMIterator{
  friend class SortedMap;

  const SortedMap& _map;
  TElem* _elems;
  int _current;

  SMIterator(const SortedMap& mapionar);

public:
  void first();
  void next();
  bool valid() const;
  TElem getCurrent() const;

  ~SMIterator();
};
