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
#include "InMemoryRepo.hh"

inline std::ostream& operator <<(std::ostream& stream, const std::vector<Task>& tasks) {
  std::copy(
    tasks.begin(),
    tasks.end(),
    std::ostream_iterator<Task>{stream, "\n"});

  return stream;
}

inline std::istream& operator >>(std::istream& stream, std::vector<Task>& tasks) {
  tasks.clear();

  std::copy(
    std::istream_iterator<Task>{stream},
    std::istream_iterator<Task>{},
    std::back_inserter(tasks));
    
  return stream;
}

class FileRepo: public InMemoryRepo {
  std::string _filePath;

  void _loadData() {
    std::ifstream inFile{_filePath};

    if (inFile.good())
      inFile >> _tasks;
    else
      _tasks.clear();
  }

  void _saveData() {
    std::ofstream outFile{_filePath};
    outFile << _tasks;
  }

public:
  FileRepo(const std::string& filePath): _filePath{filePath} { }

  const std::string& filePath() const {
    return _filePath;
  }

  virtual bool add(const Task& newTask) override {
    _loadData();
    auto isSuccesful = InMemoryRepo::add(newTask);

    if (isSuccesful)
      _saveData();

    return isSuccesful;
  }

  virtual bool update(const Task& newTask) override {
    _loadData();
    auto isSuccesful = InMemoryRepo::update(newTask);

    if (isSuccesful)
      _saveData();

    return isSuccesful;
  }

  virtual bool remove(const std::string& title) override {
    _loadData();
    auto isSuccesful = InMemoryRepo::remove(title);

    if (isSuccesful)
      _saveData();

    return isSuccesful;
  }

  virtual std::vector<Task> data() override {
    _loadData();
    return _tasks;
  }
};

#endif