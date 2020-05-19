#pragma once
#include "SortedMap.h"

//DO NOT CHANGE THIS PART
class SMIterator {
  friend class SortedMap;

  const SortedMap& _map;
  TElem* _kvps;
  std::size_t _crr;

  SMIterator(const SortedMap& mapionar);

public:
  ~SMIterator();
  
  void first();
  void next();
  bool valid() const;
  TElem getCurrent() const;
};
