#pragma once

#define NULL_TELEM -11111

typedef int TElem;

class SetIterator;

class Set {
  friend class SetIterator;

private:
  int capacity, count, maxIdx;
  TElem* array;
  TElem minElem;

public:
  //implicit constructor
  Set();

  // O(log n)
  //adds an element to the set
  //returns true if the element was added, false otherwise (if the element was already in the set and it was not added)
  bool add(TElem e);


  // O(1)
  //removes an element from the set
  //returns true if e was removed, false otherwise
  bool remove(TElem e);

  // O(1)
  //checks whether an element belongs to the set or not
  bool search(TElem elem) const;

  // O(1)
  //returns the number of elements;
  int size() const;

  // O(1)
  //check whether the set is empty or not;
  bool isEmpty() const;

  //return an iterator for the set
  SetIterator iterator() const;

  // destructor
  ~Set();
};