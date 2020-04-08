#ifndef FILE_REPO_HH
#define FILE_REPO_HH

#include <algorithm>
#include <cstddef>
#include <fstream>
#include <functional>
#include <iostream>
#include <iterator>
#include <vector>
#include "Domain.hh"
#include "Repo.hh"

inline std::ostream& operator <<(std::ostream& stream, const std::vector<Task>& tasks) {
  std::copy(tasks.begin(),
    tasks.end(),
    std::ostream_iterator<Task>{stream, "\n"});

  return stream;
}

inline std::istream& operator >>(std::istream& stream, std::vector<Task>& tasks) {
  tasks.clear();

  std::copy(std::istream_iterator<Task>{stream},
    std::istream_iterator<Task>{},
    std::back_inserter(tasks));
    
  return stream;
}


class FileRepo: public Repo {
  std::string _filePath;

  template <typename T>
  T _wrapOperation(std::function<T()> operation) {
    {
      std::ifstream inFile{_filePath};

      if (inFile.good())
        inFile >> _tasks;
      else
        _tasks.clear();
    }

    auto result = operation();
    auto data = Repo::data();

    std::ofstream outFile{_filePath};
    outFile << data;

    return result;
  }

public:
  const std::string& filePath() const {
    return _filePath;
  }

  void setFilePath(const std::string& filePath) {
    _filePath = filePath;
  }

  bool add(const Task& newTask) override {
    return _wrapOperation<bool>([&]() { return Repo::add(newTask); });
  }

  bool update(const Task& newTask) override {
    return _wrapOperation<bool>([&]() { return Repo::update(newTask); });
  }

  bool remove(const std::string& title) override {
    return _wrapOperation<bool>([&]() { return Repo::remove(title); });
  }

  std::vector<Task> data() override {
    return _wrapOperation<std::vector<Task>>([&]() { return Repo::data(); });
  }
};

#endif