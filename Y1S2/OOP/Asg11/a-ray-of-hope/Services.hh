#ifndef SERVICES_HH
#define SERVICES_HH

#include <exception>
#include <memory>
#include <stack>
#include <string>
#include <utility>
#include "Observer.hh"
#include "Repo.hh"
#include "Action.hh"
#include "TaskValidator.hh"

// Exception that is thrown if an operation is attempted
// while being in the wrong mode.
class WrongModeException : public std::exception { };

class InvalidServicesActionException : public std::exception { };

class InvalidFileTypeException : public std::exception { };

class InvalidUndoException : public std::exception { };

class InvalidRedoException : public std::exception { };

// Class that hold the state of the application.
class Services : public Observable {
  std::string _mode;
  TaskValidator _taskValidator;
  std::unique_ptr<Repo> _allTasks, _servantTasks;
  int _servantCurrentTaskIndex{-1};
  std::stack<std::unique_ptr<Action>> _done, _undone;

  void _ensureMode(const std::string& mode) const;

public:
  // Returns this instance's mode.
  const std::string& mode() const;

  // Sets the mode of this instance.
  void setMode(const std::string& mode);

  std::string filePath() const;
  std::string servantTasksFilePath() const;
  
  void setFilePath(const std::string& filePath);

  void setServantTasksFilePath(const std::string& filePath);

  // Attempts to add the specified task to the state
  // of this instance, if it does not already exist.
  // Requires mode A.
  void add(const Task& newTask);

  // Attempts to update the specified task, if it exists.
  // Requires mode A.
  void update(const Task& task);

  // Attempts to remove the specified task, if it exists.
  // Requires mode A.
  void remove(const std::string& title);

  // Returns all tasks stored in this instance.
  // Requires mode A.
  std::vector<Task> allTasks();

  void resetNext();

  // Returns the next task in this instance.
  // Requires mode B.
  std::pair<bool, Task> next();

  // Saves the task with the specified title to the servant's task list.
  // Requires mode B.
  void save(const std::string& title);

  // Returns a list of tasks filtered by the specified criteria.
  // Requires mode B.
  std::vector<Task> tasksByTimesPerformed(const std::string& type, int maxTimesPerformed);

  // Returns the servant's task list.
  // Requires mode B.
  std::vector<Task> servantTasks();
  std::vector<Task> mylist();
  
  void undo();
  void redo();
};

#endif
