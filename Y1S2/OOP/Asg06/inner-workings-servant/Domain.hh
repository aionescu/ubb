#ifndef DOMAIN_HH
#define DOMAIN_HH

#include <string>

// Represents a task to be performed by the innkeeper.
class Task {
  std::string _title;
  std::string _type;
  std::string _lastPerformed;
  int _timesPerformed;
  std::string _vision;

public:
  Task() : _title{}, _type{}, _lastPerformed{}, _timesPerformed{0}, _vision{} {}

  Task(const std::string& title, const std::string& type, const std::string& lastPerformed, int timesPerformed, const std::string& vision)
    : _title{title}, _type{type}, _lastPerformed{lastPerformed}, _timesPerformed{timesPerformed}, _vision{vision}
    {}

  Task(const Task& task) = default;
  Task& operator =(const Task& task) = default;
  ~Task() = default;

  // Returns the title of this task.
  const std::string& title() const {
    return _title;
  }

  // Returns the type of this task.
  const std::string& type() const {
    return _type;
  }

  // Returns the moment at which this task was last performed.
  const std::string& lastPerformed() const {
    return _lastPerformed;
  }

  // Returns the amount of times this task was performed.
  int timesPerformed() const {
    return _timesPerformed;
  }

  // Returns the vision of this task's outcome.
  const std::string& vision() const {
    return _vision;
  }
};

#endif