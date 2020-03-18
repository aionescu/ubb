#ifndef DOMAIN_HH
#define DOMAIN_HH

#include <string>

class Task {
  std::string _title;
  std::string _type;
  std::string _lastPerformed;
  int _timesPerformed;
  std::string _vision;

public:
  Task() = default;

  Task(std::string title, std::string type, std::string lastPerformed, int timesPerformed, std::string vision)
    : _title{title}, _type{type}, _lastPerformed{lastPerformed}, _timesPerformed{timesPerformed}, _vision{vision}
    {}

  Task(const Task& task) = default;
  Task& operator =(const Task& task) = default;
  ~Task() = default;

  std::string title() const {
    return _title;
  }

  std::string type() const {
    return _type;
  }

  std::string lastPerformed() const {
    return _lastPerformed;
  }

  int timesPerformed() const {
    return _timesPerformed;
  }

  std::string vision() const {
    return _vision;
  }
};

#endif