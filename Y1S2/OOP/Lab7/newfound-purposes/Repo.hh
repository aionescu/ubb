#ifndef REPO_HH
#define REPO_HH

#include <algorithm>
#include <vector>
#include "Domain.hh"

class Repo {
protected:
  bool _add(std::vector<Task>& tasks, const Task& newTask);
  bool _update(std::vector<Task>& tasks, const Task& newTask);
  bool _remove(std::vector<Task>& tasks, const std::string& title);

public:
  virtual ~Repo() { } /* LCOV_EXCL_LINE */

  virtual bool add(const Task& newTask) = 0;
  virtual bool update(const Task& newTask) = 0;
  virtual bool remove(const std::string& title) = 0;

  virtual std::vector<Task> data() = 0;
};

#endif