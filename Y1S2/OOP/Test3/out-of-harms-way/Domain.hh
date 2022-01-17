#ifndef DOMAIN_HH
#define DOMAIN_HH

#include <iterator>
#include <string>
#include <vector>

class Illness {
  std::string _category, _name;
  std::vector<std::string> _symptoms;

public:
  Illness(std::string category = "", std::string name = "", std::vector<std::string> symptoms = {{}});
  bool operator ==(const Illness& rhs) const;

  std::string category() const;
  std::string name() const;
  std::vector<std::string> symptoms() const;
};

std::vector<std::string> splitString(const std::string& string, char delimiter);

Illness illnessOfParts(const std::vector<std::string>& parts);

std::ostream& operator <<(std::ostream& stream, const Illness& illness);

std::istream& operator >>(std::istream& stream, Illness& illness);

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
