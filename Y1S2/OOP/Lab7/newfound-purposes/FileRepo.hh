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

class FileRepo: public Repo {
  std::string _filePath;

  std::vector<Task> _loadData() {
    std::vector<Task> tasks;
    std::ifstream inFile{_filePath};

    if (inFile.good())
      inFile >> tasks;
      
    return tasks;
  }

  void _saveData(const std::vector<Task>& tasks) {
    std::ofstream outFile{_filePath};
    outFile << tasks;
  }

public:
  FileRepo(const std::string& filePath): _filePath{filePath} { }

  const std::string& filePath() const {
    return _filePath;
  }

  virtual bool add(const Task& newTask) override {
    auto tasks = _loadData();
    auto isSuccesful = _add(tasks, newTask);

    if (isSuccesful)
      _saveData(tasks);

    return isSuccesful;
  }

  virtual bool update(const Task& newTask) override {
    auto tasks = _loadData();
    auto isSuccesful = _update(tasks, newTask);

    if (isSuccesful)
      _saveData(tasks);

    return isSuccesful;
  }

  virtual bool remove(const std::string& title) override {
    auto tasks = _loadData();
    auto isSuccesful = _remove(tasks, title);

    if (isSuccesful)
      _saveData(tasks);

    return isSuccesful;
  }

  virtual std::vector<Task> data() override {
    auto tasks = _loadData();
    return tasks;
  }
};

#endif