#pragma once

#include "List.hh"
#include "Set.h"

class SetIterator
{
  //DO NOT CHANGE THIS PART
  friend class Set;

private:
  //DO NOT CHANGE THIS PART
  const Set &set;
  Node<TElem>* _crr;
  int _idx;

  SetIterator(const Set &s);

  //TODO - Representation

public:
  void first();
  void next();

  // Best case: Theta(1)
  // Worst case: Theta(n)
  // Average case: O(n)
  void previous();
  
  TElem getCurrent();
  bool valid() const;
};