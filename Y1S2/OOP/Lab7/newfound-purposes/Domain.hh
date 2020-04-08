#ifndef DOMAIN_HH
#define DOMAIN_HH

#include <algorithm>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

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

inline void trimString(std::string& string) {
  // Trim beginning.
  string.erase(string.begin(), std::find_if(string.begin(), string.end(), [](int ch) {
    return !std::isspace(ch);
  }));

  // Trim end.
  string.erase(std::find_if(string.rbegin(), string.rend(), [](int ch) {
    return !std::isspace(ch);
  }).base(), string.end());
}

inline std::vector<std::string> splitString(const std::string& string, char delimiter) {
  std::vector<std::string> vector;

  std::stringstream stream{string};
  std::string token;
  
  while (std::getline(stream, token, delimiter)) {
    trimString(token);
    vector.push_back(token);
  }

  return vector;
}

inline Task taskOfParts(const std::vector<std::string>& parts) {
  if (parts.size() == 0)
    return Task{};

  std::string title = parts.at(0);
  std::string type = parts.at(1);
  std::string lastPerformed = parts.at(2);
  int timesPerformed = std::stoi(parts.at(3));
  std::string vision = parts.at(4);

  return Task{title, type, lastPerformed, timesPerformed, vision};
}

inline std::ostream& operator <<(std::ostream& stream, const Task& task) {
  return stream
    << task.title() << ','
    << task.type() << ','
    << task.lastPerformed() << ','
    << task.timesPerformed() << ','
    << task.vision();
}

inline std::istream& operator >>(std::istream& stream, Task& task) {
  std::string buffer;
  std::getline(stream, buffer);

  auto parts = splitString(buffer, ',');
  task = taskOfParts(parts);

  return stream;
}

#endif