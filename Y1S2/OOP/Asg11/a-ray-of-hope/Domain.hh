#ifndef DOMAIN_HH
#define DOMAIN_HH

#include <algorithm>
#include <iostream>
#include <iterator>
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
  Task(
    const std::string& title = "<title>",
    const std::string& type = "<type>",
    const std::string& lastPerformed = "<lastPerformed>",
    int timesPerformed = 0,
    const std::string& vision = "<vision>");

  bool operator ==(const Task& task) const;

  bool operator !=(const Task& task) const;

  // Returns the title of this task.
  const std::string& title() const;
  void setTitle(const std::string& title);

  // Returns the type of this task.
  const std::string& type() const;
  void setType(const std::string& type);

  // Returns the moment at which this task was last performed.
  const std::string& lastPerformed() const;
  void setLastPerformed(const std::string& lastPerformed);

  // Returns the amount of times this task was performed.
  int timesPerformed() const;
  void setTimesPerformed(int timesPerformed);

  // Returns the vision of this task's outcome.
  const std::string& vision() const;
  void setVision(const std::string& vision);
  
  std::string toString() const;
  std::vector<std::string> toParts() const;
};

void trimString(std::string& string);

std::vector<std::string> splitString(const std::string& string, char delimiter);

Task taskOfParts(const std::vector<std::string>& parts);

std::ostream& operator <<(std::ostream& stream, const Task& task);

std::istream& operator >>(std::istream& stream, Task& task);

template <typename T>
inline std::ostream& operator <<(std::ostream& stream, const std::vector<T>& vector) {
  std::copy(
    vector.begin(),
    vector.end(),
    std::ostream_iterator<T>{stream, "\n"});

  return stream;
}

template <typename T>
inline std::istream& operator >>(std::istream& stream, std::vector<T>& vector) {
  vector.clear();

  std::copy(
    std::istream_iterator<T>{stream},
    std::istream_iterator<T>{},
    std::back_inserter(vector));
    
  return stream;
}

#endif
