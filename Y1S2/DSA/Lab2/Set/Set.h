#pragma once
//DO NOT INCLUDE SETITERATOR

#include "List.hh"

//DO NOT CHANGE THIS PART
#define NULL_TELEM -11111
typedef int TElem;
class SetIterator;

class Set {
  //DO NOT CHANGE THIS PART
  friend class SetIterator;

private:
  List<TElem> _list;

public:
  //implicit constructor
  Set() = default;

  // O(n)
  //adds an element to the set
  //returns true if the element was added, false otherwise (if the element was already in the set and it was not added)
  bool add(TElem e);

  // O(n)
  //removes an element from the set
  //returns true if e was removed, false otherwise
  bool remove(TElem e);

  // O(n)
  //checks whether an element belongs to the set or not
  bool search(TElem elem) const;

  // Theta(1)
  //returns the number of elements;
  int size() const;

  // Theta(1)
  //check whether the set is empty or not;
  bool isEmpty() const;

  //return an iterator for the set
  SetIterator iterator() const;

  // destructor
  ~Set() = default;
};