#ifndef VECTOR_HH
#define VECTOR_HH

#include <cstddef>
#include <stdexcept>

template <typename T>
class Vector {
  int _capacity, _length;
  T* _data;

  bool _needsToGrow() const {
    return _capacity == 0 || _length == _capacity;
  }

  void _grow() {
    if (_capacity == 0) {
      _capacity = 1;
      _data = new T[1];
      return;
    }

    int newCapacity = _capacity * 2;
    auto newBuffer = new T[newCapacity];

    for (int i = 0; i < _length; ++i)
      newBuffer[i] = _data[i];

    delete[] _data;

    _data = newBuffer;
    _capacity = newCapacity;
  }

public:
  Vector() : _capacity{0}, _length{0}, _data{nullptr} {}

  Vector(const Vector& vector) {
    _length = vector._length;
    _capacity = vector._capacity;

    if (vector._data) {
      _data = new T[_capacity];

      for (int i = 0; i < _capacity; ++i)
        _data[i] = vector._data[i];
    } else
      _data = nullptr;
  }

  Vector& operator =(const Vector& vector) {
    this->~Vector();
    new (this) Vector(vector);
    return *this;
  }

  ~Vector() {
    _length = _capacity = 0;

    if (_data) {
      delete[] _data;
      _data = nullptr;
    }
  }

  int length() const {
    return _length;
  }

  T& operator [](int index) {
    if (index < 0 || index >= _length)
      throw std::out_of_range{"Vector<T>::operator[]: Index out of range."};

    return _data[index];
  }

  void append(T newElement) {
    if (_needsToGrow())
      _grow();

    _data[_length++] = newElement;
  }

  void remove(int index) {
    if (index < 0 || index >= _length)
      throw std::out_of_range{"Vector<T>::remove: Index out of range."};

    for (int i = index; i < _length - 1; ++i)
      _data[i] = _data[i + 1];

    --_length;
  }
};

#endif