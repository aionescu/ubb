#ifndef VECTOR_HH
#define VECTOR_HH

#include <cstddef>

template <typename T>
class Vector {
  std::size_t capacity, length;
  T* data;

public:
  Vector() : capacity{0}, length{0}, data{nullptr} {}

  std::size_t getLength() const {
    return length;
  }
};

#endif