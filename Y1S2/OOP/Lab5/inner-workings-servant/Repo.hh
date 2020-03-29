#ifndef REPO_HH
#define REPO_HH

#include <string>
#include <utility>
#include "Domain.hh"
#include "Vector.hh"

// Represents a repository of tasks.
class Repo {
  Vector<Task> _tasks;

public:
  // Attempts to add the specified task to this repository, if it
  // does not already exist.
  bool add(const Task& newTask) {
    for (auto task : _tasks)
      if (task.title() == newTask.title())
        return false;

    _tasks.append(newTask);
    return true;
  }

  // Attempts to update the specified task in this repository,
  // if it exists.
  bool update(const Task& newTask) {
    for (auto& task : _tasks)
      if (task.title() == newTask.title()) {
        task = newTask;
        return true;
      }

    return false;
  }

  // Attempts to remove the task with the specified title
  // from this repository, if it exists.
  bool remove(const std::string& title) {
    // Using basic for loop instead for range-based
    // for because the index of the found element
    // is needed.
    for (int i = 0; i < _tasks.length(); ++i)
      if (_tasks[i].title() == title) {
        _tasks.remove(i);
        return true;
      }

    return false;
  }

  // Returns a copy of this repository's data.
  const Vector<Task>& data() const {
    return _tasks;
  }
};

#endif