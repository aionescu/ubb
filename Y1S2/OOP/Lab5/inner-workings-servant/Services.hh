#ifndef SERVICES_HH
#define SERVICES_HH

#include <exception>
#include <string>
#include <utility>
#include "Repo.hh"

class WrongModeException : public std::exception {};

class Services {
  std::string _mode;
  Repo _repo;
  Repo _servantTasks;
  int _servantCurrentTaskIndex = -1;

  void _ensureMode(std::string mode) const {
    if (_mode != mode)
      throw WrongModeException{};
  }

public:
  std::string mode() const {
    return _mode;
  }

  void setMode(std::string mode) {
    _mode = mode;
  }

  bool add(Task newTask) {
    _ensureMode("A");

    return _repo.add(newTask);
  }

  bool update(Task task) {
    _ensureMode("A");

    _servantTasks.update(task);
    return _repo.update(task);
  }

  bool remove(std::string title) {
    _ensureMode("A");

    _servantTasks.remove(title);
    return _repo.remove(title);
  }

  Vector<Task> allTasks() const {
    _ensureMode("A");

    return _repo.data();
  }

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

  bool save(std::string title) {
    _ensureMode("B");

    for (auto task : _repo.data())
      if (task.title() == title)
        return _servantTasks.add(task);

    return false;
  }

  Vector<Task> tasksByTimesPerformed(std::string type, int maxTimesPerformed) const {
    _ensureMode("B");

    Vector<Task> result;
    auto data = _repo.data();

    for (auto task : _repo.data())
      if (task.type() == type && task.timesPerformed() < maxTimesPerformed)
        result.append(task);

    return result;
  }

  Vector<Task> servantTasks() const {
    _ensureMode("B");

    return _servantTasks.data();
  }
};

#endif