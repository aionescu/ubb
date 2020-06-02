#pragma once
#include "SortedMap.h"

//DO NOT CHANGE THIS PART
class SMIterator {
  friend class SortedMap;

  const SortedMap& _map;

  std::size_t _listCount;
  Node** _lists;
  
  std::size_t _crrList;
  Node* _crrNode;

  SMIterator(const SortedMap& mapionar);

public:
  ~SMIterator();
  
  void first();
  void next();
  bool valid() const;
  TElem getCurrent() const;
};
