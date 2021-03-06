#ifndef SERVICES_HH
#define SERVICES_HH

#include <stdexcept>
#include <string>
#include "Repo.hh"

class WrongModeException : public std::exception {
  const char* what() const throw() {
    return "Wrong mode.";
  }
};

class Services {
  std::string _mode;
  Repo _repo;

public:
  std::string mode() const {
    return _mode;
  }

  void setMode(std::string mode) {
    _mode = mode;
  }

  bool add(Task newTask) {
    if (_mode != "A")
      throw WrongModeException{};

    return _repo.add(newTask);
  }

  bool update(Task task) {
    if (_mode != "A")
      throw WrongModeException{};

    return _repo.update(task);
  }

  bool remove(std::string title) {
    if (_mode != "A")
      throw WrongModeException{};

    return _repo.remove(title);
  }

  Vector<Task> data() const {
    if (_mode != "A")
      throw WrongModeException{};

    return _repo.data();
  }
};

#endif