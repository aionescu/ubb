#ifndef SERVICES_HH
#define SERVICES_HH

#include <exception>
#include <string>
#include <utility>
#include "Repo.hh"

// Exception that is thrown if an operation is attempted
// while being in the wrong mode.
class WrongModeException : public std::exception {};

// Class that hold the state of the application.
class Services {
  std::string _mode;
  Repo _repo;
  Repo _servantTasks;
  int _servantCurrentTaskIndex = -1;

  void _ensureMode(const std::string& mode) const {
    if (_mode != mode)
      throw WrongModeException{};
  }

public:
  // Returns this instance's mode.
  const std::string& mode() const {
    return _mode;
  }

  // Sets the mode of this instance.
  void setMode(const std::string& mode) {
    _mode = mode;
  }

  // Attempts to add the specified task to the state
  // of this instance, if it does not already exist.
  // Requires mode A.
  bool add(const Task& newTask) {
    _ensureMode("A");

    return _repo.add(newTask);
  }

  // Attempts to update the specified task, if it exists.
  // Requires mode A.
  bool update(const Task& task) {
    _ensureMode("A");

    _servantTasks.update(task);
    return _repo.update(task);
  }

  // Attempts to remove the specified task, if it exists.
  // Requires mode A.
  bool remove(const std::string& title) {
    _ensureMode("A");

    _servantTasks.remove(title);
    return _repo.remove(title);
  }

  // Returns all tasks stored in this instance.
  // Requires mode A.
  const Vector<Task>& allTasks() const {
    _ensureMode("A");

    return _repo.data();
  }

  // Returns the next task in this instance.
  // Requires mode B.
  std::pair<bool, Task> next() {
    _ensureMode("B");

    auto data = _repo.data();

    if (data.length() == 0)
      return {false, {}};

    ++_servantCurrentTaskIndex;

    if (_servantCurrentTaskIndex == data.length())
      _servantCurrentTaskIndex = 0;

    return {true, data[_servantCurrentTaskIndex]};
  }

  // Saves the task with the specified title to the servant's task list.
  // Requires mode B.
  bool save(const std::string& title) {
    _ensureMode("B");

    for (auto task : _repo.data())
      if (task.title() == title)
        return _servantTasks.add(task);

    return false;
  }

  // Returns a list of tasks filtered by the specified criteria.
  // Requires mode B.
  Vector<Task> tasksByTimesPerformed(const std::string& type, int maxTimesPerformed) const {
    _ensureMode("B");

    Vector<Task> result;
    auto data = _repo.data();

    for (auto task : _repo.data())
      if (task.type() == type && task.timesPerformed() < maxTimesPerformed)
        result.append(task);

    return result;
  }

  // Returns the servant's task list.
  // Requires mode B.
  const Vector<Task>& servantTasks() const {
    _ensureMode("B");

    return _servantTasks.data();
  }
};

#endif