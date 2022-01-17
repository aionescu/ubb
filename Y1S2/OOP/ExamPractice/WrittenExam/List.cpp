#include <string.h>
#include <cassert>
#include <algorithm>
#include <iostream>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

class Car {
  std::string _make;
  int _horsepower;

public:
  Car(std::string make, int horsepower) : _make{make}, _horsepower{horsepower} { }

  friend std::ostream& operator <<(std::ostream& stream, const Car& car);

  bool operator <(const Car& rhs) const {
    return _horsepower < rhs._horsepower;
  }
};

std::ostream& operator <<(std::ostream& stream, const Car& car) {
  return stream << car._make << ' ' << car._horsepower;
}

template <typename T1, typename T2>
class List {
  std::vector<std::pair<T1, T2>> _vec;

public:
  void add(const T1& t1, const T2& t2) {
    _vec.push_back({t1, t2});
  }

  void printReverse(std::ostream& stream) const {
    for (auto it = _vec.rbegin(); it != _vec.rend(); ++it)
      stream << it->first << " -> " << it->second << "; ";

    stream << '\n';
  }

  // Sorts the elements of the list according to the first component of each element.
  // Requiements: Template parameter T1 must satisfy the LessThanComparable concept.
  // Throws: std::runtime_error if the list is empty.
  void sortByFirstComponent() {
    if (_vec.empty())
      throw std::runtime_error{"List is empty!"};

    std::sort(_vec.begin(), _vec.end(),
      [](const std::pair<T1, T2>& a, const std::pair<T1, T2>& b) {
        return a.first < b.first;
      });
  }

  void sortBySecondComponent() {
    if (_vec.empty())
      throw std::runtime_error{"List is empty!"};

    std::sort(_vec.begin(), _vec.end(),
      [](const std::pair<T1, T2>& a, const std::pair<T1, T2>& b) {
        return a.second < b.second;
      });
  }
};

void function1() {
  List<Car, int> list1;

  try {
    list1.sortByFirstComponent();
    assert(false);
  }
  catch (std::runtime_error& e) {
    assert(strcmp(e.what(), "List is empty!") == 0);
  }

  list1.add(Car{"Audi", 20}, 8);
  list1.add(Car{"Volkswagen", 10}, 9);
  list1.add(Car{"Bentley", 300}, 10);

  list1.sortByFirstComponent();
  list1.printReverse(std::cout); // prints: Bentley 300 -> 10; Audi 20 -> 8; Volkswagen 10 -> 9 
  list1.sortBySecondComponent();
  list1.printReverse(std::cout); // prints: Bentley 300 -> 10; Volkswagen 10 -> 9; Audi 20 -> 8;
}

int main() {
  function1();
}