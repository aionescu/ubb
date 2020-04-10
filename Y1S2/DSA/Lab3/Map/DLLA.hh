#ifndef DLLA_HH
#define DLLA_HH

template <typename T>
struct Node {
  T value;
  int prev, next;
};

template <typename T>
class DLLA {
  Node<T>* _array;
  int _capacity, _head, _tail, _firstEmpty, _length;

  bool needsToGrow() const {
    return _capacity == 0 || _length == _capacity;
  }

  void grow() {
    if (_capacity == 0) {
      _array = new Node<T>[1];
      _capacity = _length = 1;

      _firstEmpty = 0;
      _array[0].prev = _array[0].next = -1;

      return;
    }

    _capacity *= 2;

    auto newBuffer = new Node<T>[_capacity];

    for (int i = 0; i < _length; ++i)
      newBuffer[i] = _array[i];

    delete[] _array;
    _array = newBuffer;
  }

public:
  DLLA() {
    _capacity = _length = 0;
    _head = _tail = _firstEmpty = -1;
    _array = nullptr;
  }
};

#endif