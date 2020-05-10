#ifndef REPO_HH
#define REPO_HH

#include <algorithm>
#include <exception>
#include <vector>
#include "Domain.hh"

class InvalidRepoActionException: public std::exception { };

class Repo {
protected:
  void _add(std::vector<Task>& tasks, const Task& newTask);
  void _update(std::vector<Task>& tasks, const Task& newTask);
  void _remove(std::vector<Task>& tasks, const std::string& title);

public:
  virtual ~Repo() { } /* LCOV_EXCL_LINE */

  virtual void add(const Task& newTask) = 0;
  virtual void update(const Task& newTask) = 0;
  virtual void remove(const std::string& title) = 0;

  virtual std::vector<Task> data() = 0;
};

#endif