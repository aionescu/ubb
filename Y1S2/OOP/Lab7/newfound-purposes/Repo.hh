#ifndef REPO_HH
#define REPO_HH

#include <algorithm>
#include <string>
#include <utility>
#include <vector>
#include "Domain.hh"

// Represents a repository of tasks.
class Repo {
protected:
  std::vector<Task> _tasks;

public:
  Repo(const std::vector<Task>& tasks = {}) : _tasks{tasks} {}

  // Attempts to add the specified task to this repository, if it
  // does not already exist.
  virtual bool add(const Task& newTask) {
    auto predicate = [&](const Task& task) { return task.title() == newTask.title(); };
    auto findResult = std::find_if(_tasks.begin(), _tasks.end(), predicate);

    if (findResult != _tasks.end())
      return false;

    _tasks.push_back(newTask);
    return true;
  }

  // Attempts to update the specified task in this repository,
  // if it exists.
  virtual bool update(const Task& newTask) {
    auto predicate = [&](const Task& task) { return task.title() == newTask.title(); };
    auto findResult = std::find_if(_tasks.begin(), _tasks.end(), predicate);

    if (findResult == _tasks.end())
      return false;

    *findResult = newTask;
    return true;
  }

  // Attempts to remove the task with the specified title
  // from this repository, if it exists.
  virtual bool remove(const std::string& title) {
    auto predicate = [&](const Task& task) { return task.title() == title; };

    auto findResult = std::find_if(_tasks.begin(), _tasks.end(), predicate);

    if (findResult == _tasks.end())
      return false;

    auto removeResult = std::remove_if(_tasks.begin(), _tasks.end(), predicate);
    _tasks.erase(removeResult, _tasks.end());

    return true;
  }

  // Returns a copy of this repository's data.
  virtual std::vector<Task> data() {
    return _tasks;
  }
};

#endif