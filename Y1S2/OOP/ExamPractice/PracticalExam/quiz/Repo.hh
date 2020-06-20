#ifndef REPO_HH
#define REPO_HH

#include <algorithm>
#include <exception>
#include <fstream>
#include <iostream>
#include <iterator>
#include <functional>
#include <vector>
#include "Domain.hh"

template <typename T>
inline std::ostream& operator <<(std::ostream& stream, const std::vector<T>& vector) {
  std::copy(
    vector.begin(),
    vector.end(),
    std::ostream_iterator<T>{stream, "\n"});

  return stream;
}

template <typename T>
inline std::istream& operator >>(std::istream& stream, std::vector<T>& vector) {
  vector.clear();

  std::copy(
    std::istream_iterator<T>{stream},
    std::istream_iterator<T>{},
    std::back_inserter(vector));
    
  return stream;
}

class InexistentItem : public std::exception { };

template <typename TEntity>
class Repo {
  std::vector<TEntity> _data;

public:
  virtual ~Repo() = default;
  
  void loadFromFile(std::string path) {
    std::ifstream{path} >> _data;
  }

  void saveToFile(std::string path) const {
    std::ofstream{path} << _data;
  }

  void add(TEntity entity) {
    _data.push_back(entity);
  }

  void update(TEntity oldEntity, TEntity newEntity) {
    auto it = std::find(_data.begin(), _data.end(), oldEntity);

    if (it == _data.end())
      throw InexistentItem{};
    else
      *it = newEntity;
  }

  void remove(TEntity entity) {
    auto it = std::find(_data.begin(), _data.end(), entity);
    
    if (it == _data.end())
      throw InexistentItem{};
    else
      _data.erase(it);
  }

  TEntity& getByPredicate(std::function<bool(const TEntity&)> predicate) {
    auto it = std::find_if(_data.begin(), _data.end(), predicate);

    if (it == _data.end())
      throw InexistentItem{};
    else
      return *it;
  }

  std::vector<TEntity> getAllData() const {
    return _data;
  }
};

class Repository : public Repo<Question>, public Repo<Participant> { };

#endif