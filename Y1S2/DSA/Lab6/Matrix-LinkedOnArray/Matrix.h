#pragma once

#include <functional>
#include <stdexcept>

//DO NOT CHANGE THIS PART
typedef int TElem;
#define NULL_TELEM 0

struct Triple {
  int line;
  int column;
  TElem value;

  bool operator ==(const Triple& other) const {
    return
      line == other.line
      && column == other.column;
  }

  bool operator !=(const Triple& other) const {
    return !(*this == other);
  }

  bool operator <(const Triple& other) const {
    if (line == other.line)
      return column < other.column;
    else
      return line < other.line;
  }

  bool operator <=(const Triple& other) const {
    return !(*this > other);
  }

  bool operator >(const Triple& other) const {
    if (line == other.line)
      return column > other.column;
    else
      return line > other.line;
  }

  bool operator >=(const Triple& other) const {
    return !(*this < other);
  }
};

#define NULL_TRIPLE Triple{-1, -1, NULL_TELEM}

struct Node {
  Triple triple;
  int left, right;
};

class Matrix {
private:
  int lines, columns;
  Node* array;
  int capacity, firstEmpty, root;

  TElem add(int i, int j, TElem value);
  TElem remove(int i, int j);

  void resize();

  int allocate();
  void deallocate(int);

  bool canInsertAfter(int node, Triple triple);
  int findParentOfMaximum(int startingNode);

public:
  //constructor
  Matrix(int nrLines, int nrCols);
  ~Matrix();

  // Th(1)
  //returns the number of lines
  int nrLines() const;

  // Th(1)
  //returns the number of columns
  int nrColumns() const;

  // Th(h)
  //returns the element from line i and column j (indexing starts from 0)
  //throws exception if (i,j) is not a valid position in the Matrix
  TElem element(int i, int j) const;

  // Th(h)
  //modifies the value from line i and column j
  //returns the previous value from the position
  //throws exception if (i,j) is not a valid position in the Matrix
  TElem modify(int i, int j, TElem e);
};
