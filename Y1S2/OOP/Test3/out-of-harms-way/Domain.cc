#include <algorithm>
#include <sstream>
#include "Domain.hh"

Illness::Illness(std::string category, std::string name, std::vector<std::string> symptoms)
  : _category{category}, _name{name}, _symptoms{symptoms}
{ }

bool Illness::operator ==(const Illness& rhs) const {
  return
    _category == rhs._category
    && _name == rhs._name
    && _symptoms == rhs._symptoms;
}

std::string Illness::category() const {
  return _category;
}

std::string Illness::name() const {
  return _name;
}

std::vector<std::string> Illness::symptoms() const {
  return _symptoms;
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

Illness illnessOfParts(const std::vector<std::string>& parts) {
  if (parts.size() == 0)
    return Illness{};

  std::string category = parts.at(0);
  std::string name = parts.at(1);
  std::string symptoms = parts.at(2);

  return Illness{category, name, splitString(symptoms, ',')};
}

std::istream& operator >>(std::istream& stream, Illness& illness) {
  std::string buffer;
  std::getline(stream, buffer);

  auto parts = splitString(buffer, '|');
  illness = illnessOfParts(parts);

  return stream;
}
