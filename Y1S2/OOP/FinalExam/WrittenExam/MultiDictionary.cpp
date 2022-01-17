#include <string.h>
#include <cassert>
#include <algorithm>
#include <iostream>
#include <stdexcept>
#include <utility>
#include <vector>

using std::string;

template <typename T1, typename T2>
class MultiDictionary {
  std::vector<std::pair<T1, T2>> _vec;

public:
  MultiDictionary& add(const T1& t1, const T2& t2) {
    auto it = std::find(_vec.begin(), _vec.end(), std::pair<T1, T2>{t1, t2});

    if (it == _vec.end())
      _vec.push_back(std::pair<T1, T2>{t1, t2});

    return *this;
  }

  // Member function that erases the specified key-value pair from the MultiDictionary.
  // Input: t1: T1 - the key of the pair to remove, t2: T2 - the value of the pair to remove.
  // Output: -
  // Postconditions: If the key existed in the MultiDictionary, it was removed.
  // Throws: std::runtime_error if either the key-value pair is not present in the MultiDictionary, or
  // if no pair with the specified key is present in the MultiDictionary.
  void erase(const T1& t1, const T2& t2) {
    auto it = std::find_if(_vec.begin(), _vec.end(),
      [&](const std::pair<T1, T2>& kvp) { return kvp.first == t1; });

    if (it == _vec.end())
      throw std::runtime_error{"Key does not exist!"};

    it = std::find(_vec.begin(), _vec.end(), std::pair<T1, T2>{t1, t2});

    if (it == _vec.end())
      throw std::runtime_error{"Given value does not exist for given key!"};

    _vec.erase(it);
  }

  void print(std::ostream& stream) {
    if (_vec.empty())
      return;

    stream << _vec.at(0).first << ' ' << _vec.at(0).second;

    for (std::size_t i = 1; i < _vec.size(); ++i)
      stream << "; " << _vec.at(i).first << ' ' << _vec.at(i).second;

    stream << '\n';
  }

  std::size_t sizeForKey(const T1& key) const {
    return std::count_if(_vec.begin(), _vec.end(),
      [&](const std::pair<T1, T2>& kvp) { return kvp.first == key; });
  }
};

class Person {
  std::string _name;
  int _age;

public:
  Person(std::string name, int age) : _name{name}, _age{age} { }

  bool operator ==(const Person& rhs) const {
    return _name == rhs._name && _age == rhs._age;
  }

  friend std::ostream& operator <<(std::ostream& stream, const Person& person);
};

std::ostream& operator <<(std::ostream& stream, const Person& person) {
  return stream << person._name << " is " << person._age << " years old";
}

void function2()
{
  MultiDictionary<int, string> d1{};
  d1.add(1, "a").add(2, "b").add(1, "c").add(3, "d");
  d1.print(std::cout); // prints 1 a; 2 b; 1 c; 3 d
  try {
    d1.erase(4, "w");
    assert(false); 	
  }
  catch (std::runtime_error& e) {
    try {
    assert(strcmp(e.what(), "Key does not exist!") == 0);
    d1.erase(2, "w");
    assert(false);   }
    catch (std::runtime_error& e) {
    assert(strcmp(e.what(), "Given value does not exist for given key!") == 0);
    }
  }

  d1.erase(1, "a");
  d1.print(std::cout); // prints 2 b; 1 c; 3 d
  MultiDictionary<string, Person> d2{};
  Person p1{ "Bianca", 20 }; Person p2{ "Andrei", 21 };
  d2.add("a", p1).add("a", p2);
  d2.print(std::cout); // prints a Bianca is 20 years old; a Andrei is 21 years old;.
  assert(d2.sizeForKey("a") == 2);  	
  assert(d2.sizeForKey("b") == 0);
}

int main() {
  function2();
}