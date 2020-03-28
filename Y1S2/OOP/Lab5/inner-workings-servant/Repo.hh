#ifndef REPO_HH
#define REPO_HH

#include <string>
#include <utility>
#include "Domain.hh"
#include "Vector.hh"

class Repo {
  Vector<Task> _tasks;

public:
  bool add(Task newTask) {
    for (auto task : _tasks)
      if (task.title() == newTask.title())
        return false;

    _tasks.append(newTask);
    return true;
  }

  bool update(Task newTask) {
    for (auto& task : _tasks)
      if (task.title() == newTask.title()) {
        task = newTask;
        return true;
      }

    return false;
  }

  bool remove(std::string title) {
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

  Vector<Task> data() const {
    return _tasks;
  }
};

#endif