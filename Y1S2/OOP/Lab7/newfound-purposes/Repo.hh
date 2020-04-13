#ifndef REPO_HH
#define REPO_HH

#include <algorithm>
#include <vector>
#include "Domain.hh"

class Repo {
protected:
  bool _add(std::vector<Task>& tasks, const Task& newTask) {
    auto predicate = [&](const Task& task) { return task.title() == newTask.title(); };
    auto findResult = std::find_if(tasks.begin(), tasks.end(), predicate);

    if (findResult != tasks.end())
      return false;

    tasks.push_back(newTask);
    return true;
  }

  bool _update(std::vector<Task>& tasks, const Task& newTask) {
    auto predicate = [&](const Task& task) { return task.title() == newTask.title(); };
    auto findResult = std::find_if(tasks.begin(), tasks.end(), predicate);

    if (findResult == tasks.end())
      return false;

    *findResult = newTask;
    return true;
  }

  bool _remove(std::vector<Task>& tasks, const std::string& title) {
    auto predicate = [&](const Task& task) { return task.title() == title; };

    auto findResult = std::find_if(tasks.begin(), tasks.end(), predicate);

    if (findResult == tasks.end())
      return false;

    auto removeResult = std::remove_if(tasks.begin(), tasks.end(), predicate);
    tasks.erase(removeResult, tasks.end());

    return true;
  }

public:
  virtual ~Repo() { } /* LCOV_EXCL_LINE */

  virtual bool add(const Task& newTask) = 0;
  virtual bool update(const Task& newTask) = 0;
  virtual bool remove(const std::string& title) = 0;

  virtual std::vector<Task> data() = 0;
};

#endif