#ifndef SERVICES_HH
#define SERVICES_HH

#include <algorithm>
#include <cstddef>
#include <exception>
#include <iterator>
#include <string>
#include <utility>
#include "Repo.hh"
#include "FileRepo.hh"

// Exception that is thrown if an operation is attempted
// while being in the wrong mode.
class WrongModeException: public std::exception {};

// Class that hold the state of the application.
class Services {
  std::string _mode;
  FileRepo _allTasks;
  Repo _servantTasks;
  int _servantCurrentTaskIndex;

  void _ensureMode(const std::string& mode) const {
    if (_mode != mode)
      throw WrongModeException{};
  }

public:
  Services(const std::string& mode = "", const std::string& filePath = "")
  : _mode{mode},
    _allTasks{filePath},
    _servantTasks{},
    _servantCurrentTaskIndex{-1}
  {}

  // Returns this instance's mode.
  const std::string& mode() const {
    return _mode;
  }

  // Sets the mode of this instance.
  void setMode(const std::string& mode) {
    _mode = mode;
  }

  const std::string& filePath() const {
    return _allTasks.filePath();
  }

  void setFilePath(const std::string filePath) {
    _allTasks.setFilePath(filePath);
  }

  // Attempts to add the specified task to the state
  // of this instance, if it does not already exist.
  // Requires mode A.
  bool add(const Task& newTask) {
    _ensureMode("A");

    return _allTasks.add(newTask);
  }

  // Attempts to update the specified task, if it exists.
  // Requires mode A.
  bool update(const Task& task) {
    _ensureMode("A");

    _servantTasks.update(task);
    return _allTasks.update(task);
  }

  // Attempts to remove the specified task, if it exists.
  // Requires mode A.
  bool remove(const std::string& title) {
    _ensureMode("A");

    _servantTasks.remove(title);
    return _allTasks.remove(title);
  }

  // Returns all tasks stored in this instance.
  // Requires mode A.
  std::vector<Task> allTasks() {
    _ensureMode("A");

    return _allTasks.data();
  }

  // Returns the next task in this instance.
  // Requires mode B.
  std::pair<bool, Task> next() {
    _ensureMode("B");

    auto data = _allTasks.data();

    if (data.size() == 0)
      return {false, {}};

    ++_servantCurrentTaskIndex;

    if (_servantCurrentTaskIndex == (int)data.size())
      _servantCurrentTaskIndex = 0;

    return {true, data.at(_servantCurrentTaskIndex)};
  }

  // Saves the task with the specified title to the servant's task list.
  // Requires mode B.
  bool save(const std::string& title) {
    _ensureMode("B");
    
    for (const auto& task : _allTasks.data())
      if (task.title() == title)
        return _servantTasks.add(task);

    return false;
  }

  // Returns a list of tasks filtered by the specified criteria.
  // Requires mode B.
  std::vector<Task> tasksByTimesPerformed(const std::string& type, int maxTimesPerformed) {
    _ensureMode("B");

    std::vector<Task> result;
    auto data = _allTasks.data();

    auto predicate = [&](const Task& task) {
      return task.type() == type && task.timesPerformed() < maxTimesPerformed;
    };

    std::copy_if(data.begin(), data.end(), std::back_inserter(result), predicate);
    return result;
  }

  // Returns the servant's task list.
  // Requires mode B.
  std::vector<Task> servantTasks() {
    _ensureMode("B");

    return _servantTasks.data();
  }
};

#endif