#ifndef REPO_HH
#define REPO_HH

#include <vector>
#include "Domain.hh"

class Repo {
public:
  virtual ~Repo() { } /* LCOV_EXCL_LINE */ 

  virtual bool add(const Task& newTask) = 0;
  virtual bool update(const Task& newTask) = 0;
  virtual bool remove(const std::string& title) = 0;

  virtual std::vector<Task> data() = 0;
};

#endif