#ifndef DOMAIN_HH
#define DOMAIN_HH

#include <algorithm>
#include <iostream>
#include <iterator>
#include <sstream>
#include <string>
#include <tuple>
#include <vector>

void trimString(std::string& string);
std::vector<std::string> splitString(const std::string& string, char delimiter);

template <typename T>
inline void fromString(std::string string, T& buf);

template <typename T>
inline void fromString(std::string string, T& buf) {
  std::istringstream{string} >> buf;
}

template <>
inline void fromString<std::string>(std::string string, std::string& buf) {
  buf = string;
}

template <typename TSelf, typename... TFields>
class Entity {
protected:
  std::tuple<TFields...> _data;

public:
  Entity() { }
  Entity(TFields... fields) : _data{fields...} { }
  virtual ~Entity() = default;

  template <std::size_t TIdx>
  auto get() const {
    return std::get<TIdx>(_data);
  }

  template <std::size_t TIdx>
  void set(std::tuple_element_t<TIdx, decltype(_data)> value) {
    std::get<TIdx>(_data) = value;
  }

  bool operator ==(const Entity& rhs) const {
    return _data == rhs._data;
  }

  bool operator !=(const Entity& rhs) const {
    return _data != rhs._data;
  }

  std::string toString() const {
    std::ostringstream ss;
    ss << *this;
    
    return ss.str();
  }

  friend std::istream& operator >>(std::istream& stream, Entity& entity) {
    if constexpr (sizeof...(TFields) > 0) {
      std::string buf;
      std::getline(stream, buf);
      auto parts = splitString(buf, ',');

      if (parts.empty())
        return stream;
        
      std::size_t idx = 0;

      std::apply([&](auto&... fields) {
        (fromString(parts.at(idx++), fields), ...);
      }, entity._data);
    }

    return stream;
  }

  friend std::ostream& operator <<(std::ostream& stream, const Entity& entity) {
    if constexpr (sizeof...(TFields) > 0)
      std::apply([&](const auto& fst, const auto&... rest) {
          stream << fst;
          ((stream << "," << rest), ...);
        }, entity._data);

    return stream;
  }
};

class Question : public Entity<Question, int, std::string, std::string, int> {
public:
  using Entity::Entity;

  enum {
    id,
    text,
    correctAnswer,
    score 
  };
};

class Participant : public Entity<Participant, std::string, int> {
public:
  using Entity::Entity;

  enum {
    name,
    score
  };
};

#endif