#ifndef LIST_HH
#define LIST_HH

#include <functional>

// Node is a simple POD type, all logic is implemented in List.
template <typename T>
struct Node {
  T value;
  Node* next;
};

template <typename T>
class List {
  int _length;
  Node<T>* _head;
  Node<T>* _last;

public:
  List() : _length{0}, _head{nullptr}, _last{nullptr} {}

  List(const List& list) {
    _length = list._length;

    if (_length == 0) {
      _head = nullptr;
      _last = nullptr;
      return;
    }

    _head = _last = new Node<T>{list._head->value, nullptr};

    for (auto crr = list._head->next; crr; crr = crr->next)
      _last = _last->next = new Node<T>{crr->value, nullptr};
  }

  List& operator =(const List& list) {
    this->~List();
    new (this) List(list);

    return *this;
  }

  ~List() {
    auto crr = _head;

    while (crr) {
      auto next = crr->next;
      delete crr;
      crr = next;
    }

    _length = 0;
    _head = nullptr;
    _last = nullptr;
  }

  int length() const {
    return _length;
  }

  Node<T>* head() const {
    return _head;
  }

  void add(T elem) {
    if (!_head) {
      _last = _head = new Node<T>{elem, nullptr};
      _length = 1;
      return;
    }

    _last = _last->next = new Node<T>{elem, nullptr};
    ++_length;
  }

  bool remove(T elem) {
    if (!_head)
      return false;

    if (_head->value == elem) {
      auto old = _head;

      _head = _head->next;

      delete old;
      --_length;

      return true;
    }

    auto prev = _head;
    auto crr = _head->next;

    while (crr) {
      if (crr->value == elem) {
        if (crr == _last)
          _last = prev;

        prev->next = crr->next;
        delete crr;

        --_length;
        return true;
      }

      prev = crr;
      crr = crr->next;
    }

    return false;
  }

  bool contains(T elem) const {
    auto crr = _head;

    while (crr) {
      if (crr->value == elem)
        return true;

      crr = crr->next;
    }

    return false;
  }

  Node<T>* nodeAt(int index) const {
    if (index < 0 || index >= _length)
      return nullptr;

    auto crr = _head;

    while (crr && index--) {
      crr = crr->next;
    }

    return crr;
  }

  void iter(std::function<void(T)> f) {
    auto crr = _head;

    while (crr) {
      f(crr->value);
      crr = crr->next;
    }
  }
};

#endif