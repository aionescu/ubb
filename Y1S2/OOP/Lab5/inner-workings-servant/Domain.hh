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
  Task() : _title{}, _type{}, _lastPerformed{}, _timesPerformed{0}, _vision{} {}

  Task(std::string title, std::string type, std::string lastPerformed, int timesPerformed, std::string vision)
    : _title{title}, _type{type}, _lastPerformed{lastPerformed}, _timesPerformed{timesPerformed}, _vision{vision}
    {}

  Task(const Task& task) = default;
  Task& operator =(const Task& task) = default;
  ~Task() = default;

  const std::string& title() const {
    return _title;
  }

  const std::string& type() const {
    return _type;
  }

  const std::string& lastPerformed() const {
    return _lastPerformed;
  }

  int timesPerformed() const {
    return _timesPerformed;
  }

  const std::string& vision() const {
    return _vision;
  }
};

#endif