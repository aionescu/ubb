#include <cassert>
#include <iostream>
#include <vector>
#include "List.hh"

template <typename T>
std::vector<T> toVector(List<T> list) {
  std::vector<T> result;

  list.iter([&](T t) { result.push_back(t); });

  return result;
}

template <typename T>
void printList(List<T> list) {
  list.iter([](T t) { std::cout << t << ' '; });
  std::cout << '\n';
}

void testAdd() {
  List<int> l;

  l.add(2);
  l.add(3);
  l.add(4);

  printList(l);
}

void testRemove() {
  List<int> l;

  for (int i = 0; i < 10; ++i) {
    l.add(i);
  }
  
  printList(l);

  l.remove(5);
  printList(l);

  l.remove(8);
  printList(l);

  l.remove(0);
  printList(l);
}

void testCopyCtor() {
  List<int> l;

  for (int i = 0; i < 1000000; ++i)
    l.add(i);

  std::cout << l.length() << ' ' << std::flush;

  List<int> l2{l};

  std::cout << l2.length() << ' ' << std::flush;
}

int main() {
  testAdd();
  testRemove();
  testCopyCtor();
}