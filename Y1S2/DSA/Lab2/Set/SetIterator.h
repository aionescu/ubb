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

  SetIterator(const Set &s);

  //TODO - Representation

public:
  void first();
  void next();
  TElem getCurrent();
  bool valid() const;
};