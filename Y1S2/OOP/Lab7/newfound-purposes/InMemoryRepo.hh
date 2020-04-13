#ifndef IN_MEMORY_REPO_HH
#define IN_MEMORY_REPO_HH

#include <algorithm>
#include <string>
#include <utility>
#include <vector>
#include "Domain.hh"
#include "Repo.hh"

// Represents a repository of tasks.
class InMemoryRepo: public Repo {
protected:
  std::vector<Task> _tasks;

public:
  // Attempts to add the specified task to this repository, if it
  // does not already exist.
  virtual bool add(const Task& newTask) override {
    return _add(_tasks, newTask);
  }

  // Attempts to update the specified task in this repository,
  // if it exists.
  virtual bool update(const Task& newTask) override {
    return _update(_tasks, newTask);
  }

  // Attempts to remove the task with the specified title
  // from this repository, if it exists.
  virtual bool remove(const std::string& title) override {
    return _remove(_tasks, title);
  }

  // Returns a copy of this repository's data.
  virtual std::vector<Task> data() override {
    return _tasks;
  }
};

#endif