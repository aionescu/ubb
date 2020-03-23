#ifndef REPO_HH
#define REPO_HH

#include <string>
#include "Domain.hh"
#include "Vector.hh"

class Repo {
  Vector<Task> _tasks;

public:
  bool add(Task newTask) {
    for (int i = 0; i < _tasks.length(); ++i)
      if (_tasks[i].title() == newTask.title())
        return false;

    _tasks.append(newTask);
    return true;
  }

  bool update(Task task) {
    for (int i = 0; i < _tasks.length(); ++i)
      if (_tasks[i].title() == task.title()) {
        _tasks[i] = task;
        return true;
      }

    return false;
  }

  bool remove(std::string title) {
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