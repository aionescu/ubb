#include <sstream>
#include "Domain.hh"

Task::Task(
  const std::string& title,
  const std::string& type,
  const std::string& lastPerformed,
  int timesPerformed,
  const std::string& vision)
  : _title{title},
    _type{type},
    _lastPerformed{lastPerformed},
    _timesPerformed{timesPerformed},
    _vision{vision}
  { }

bool Task::operator ==(const Task& task) const {
  return
    _title == task._title
    && _type == task._type
    && _lastPerformed == task._lastPerformed
    && _timesPerformed == task._timesPerformed
    && _vision == task._vision;
}

bool Task::operator !=(const Task& task) const {
  return !(*this == task);
}

const std::string& Task::title() const {
  return _title;
}

const std::string& Task::type() const {
  return _type;
}

const std::string& Task::lastPerformed() const {
  return _lastPerformed;
}

int Task::timesPerformed() const {
  return _timesPerformed;
}

const std::string& Task::vision() const {
  return _vision;
}

void trimString(std::string& string) {
  auto isNotSpace = [](char character) { return !std::isspace(character); };
  
  // Trim beginning.
  string.erase(string.begin(), std::find_if(string.begin(), string.end(), isNotSpace));

  // Trim end.
  string.erase(std::find_if(string.rbegin(), string.rend(), isNotSpace).base(), string.end());
}

std::vector<std::string> splitString(const std::string& string, char delimiter) {
  std::vector<std::string> vector;

  std::istringstream stream{string};
  std::string token;
  
  while (std::getline(stream, token, delimiter)) {
    trimString(token);
    vector.push_back(token);
  }

  return vector;
}

Task taskOfParts(const std::vector<std::string>& parts) {
  if (parts.size() == 0)
    return Task{};

  std::string title = parts.at(0);
  std::string type = parts.at(1);
  std::string lastPerformed = parts.at(2);
  int timesPerformed = std::stoi(parts.at(3));
  std::string vision = parts.at(4);

  return Task{title, type, lastPerformed, timesPerformed, vision};
}

std::istream& operator >>(std::istream& stream, Task& task) {
  std::string buffer;
  std::getline(stream, buffer);

  auto parts = splitString(buffer, ',');
  task = taskOfParts(parts);

  return stream;
}