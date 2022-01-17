#include <string.h>
#include <cassert>
#include <stack>
#include <stdexcept>
#include <string>

template <typename T>
class Stack {
  std::stack<T> _stack;
  std::size_t _cap;

public:
  Stack(std::size_t capacity) : _stack{}, _cap{capacity} { }
  
  // Returns the stack's maximum capacity.
  std::size_t getMaxCapacity() const noexcept {
    return _cap;
  }

  // Removes, and returns, the element at the top of the stack.
  // Throws: std::runtime_error if the stack is empty.
  T pop() {
    if (_stack.empty())
      throw std::runtime_error{"Stack is empty!"};

    auto top = _stack.top();
    _stack.pop();

    return top;
  }

  // Constructs a new stack containing the current elements of the stack, as well as the new value.
  // Throws: std::runtime_error if the stack is at full capacity.
  Stack operator +(const T& value) const {
    if (_stack.size() == _cap)
      throw std::runtime_error{"Stack is full!"};

    Stack temp{*this}; // Calling default copy constructor
    temp._stack.push(value);

    return temp;   
  }
};

void testStack() {
  Stack<std::string> s{2};
  assert(s.getMaxCapacity() == 2);
  
  try {
    s = s + "examination";
    s = s + "oop";
    s = s + "test";
  } catch (std::exception& e) {
    assert(strcmp(e.what(), "Stack is full!") == 0);
  }
  
  assert(s.pop() == "oop");
  assert(s.pop() == "examination");
}

int main() {
  testStack();
}