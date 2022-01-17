#ifndef SERVICES_HH
#define SERVICES_HH

#include <exception>
#include <string>
#include <utility>
#include "InMemoryRepo.hh"
#include "FileRepo.hh"

// Exception that is thrown if an operation is attempted
// while being in the wrong mode.
class WrongModeException: public std::exception { };

// Class that hold the state of the application.
class Services {
  std::string _mode;
  FileRepo _allTasks{""};
  InMemoryRepo _servantTasks;
  int _servantCurrentTaskIndex{-1};

  void _ensureMode(const std::string& mode) const;

public:
  // Returns this instance's mode.
  const std::string& mode() const;

  // Sets the mode of this instance.
  void setMode(const std::string& mode);

  const std::string& filePath() const;

  void setFilePath(const std::string filePath);

  // Attempts to add the specified task to the state
  // of this instance, if it does not already exist.
  // Requires mode A.
  bool add(const Task& newTask);

  // Attempts to update the specified task, if it exists.
  // Requires mode A.
  bool update(const Task& task);

  // Attempts to remove the specified task, if it exists.
  // Requires mode A.
  bool remove(const std::string& title);

  // Returns all tasks stored in this instance.
  // Requires mode A.
  std::vector<Task> allTasks();

  // Returns the next task in this instance.
  // Requires mode B.
  std::pair<bool, Task> next();

  // Saves the task with the specified title to the servant's task list.
  // Requires mode B.
  bool save(const std::string& title);

  // Returns a list of tasks filtered by the specified criteria.
  // Requires mode B.
  std::vector<Task> tasksByTimesPerformed(const std::string& type, int maxTimesPerformed);

  // Returns the servant's task list.
  // Requires mode B.
  std::vector<Task> servantTasks();
};

#endif