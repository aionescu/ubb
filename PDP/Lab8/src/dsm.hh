#ifndef DSM_HH
#define DSM_HH

#include <map>
#include <set>
#include <mutex>
#include <thread>
#include "mpi.hh"

template <typename T>
std::ostream &operator <<(std::ostream &os, const std::set<T> &set) {
  os << "Set[";

  for (auto it = set.begin(); it != set.end(); ++it) {
    if (it != set.begin())
      os << ", ";

    os << *it;
  }

  return os << "]";
}

template <typename K, typename V>
std::ostream &operator <<(std::ostream &os, const std::map<K, V> &map) {
  os << "Map[";

  for (auto it = map.begin(); it != map.end(); ++it) {
    if (it != map.begin())
      os << ", ";

    os << *it.first << ": " << *it.second;
  }

  return os << "]";
}

class DsmException: public std::exception {
private:
  std::string _msg;

public:
  DsmException(std::string msg): _msg{msg} {}

  const char *what() const noexcept override {
    return _msg.c_str();
  }
};

class Dsm {
private:
  int _nproc = mpi_nproc();
  int _rank = mpi_rank();
  std::map<std::string, std::set<int>> _subscriptions;
  std::map<std::string, int> _variables;
  std::recursive_mutex _mutex;

  void _require_subscribed(std::string variable, int process) {
    if (_subscriptions[variable].find(process) == _subscriptions[variable].end())
      throw DsmException{"Process " + std::to_string(process) + " is not subscribed to variable " + variable};
  }

  void _send_to_subscribers(std::string variable, Msg msg) {
    std::cout << _rank << " > Sending " << msg << " to " << _subscriptions[variable] << std::endl;

    for (int i = 0; i < _nproc; ++i)
      if (_rank != i && _subscriptions[variable].count(i))
        mpi_send_msg(msg, i, 0);
  }

  void _send_to_all(Msg msg) {
    std::cout << _rank << " > Sending " << msg << " to all" << std::endl;

    for (int i = 0; i < _nproc; ++i)
      if (_rank != i || std::holds_alternative<Close>(msg))
        mpi_send_msg(msg, i, 0);
  }

public:
  void update(std::string variable, int value) {
    std::lock_guard guard(_mutex);
    _require_subscribed(variable, _rank);

    _variables[variable] = value;
    _send_to_subscribers(variable, Update{variable, value});
  }

  void compare_exchange(std::string variable, int expected, int value) {
    std::lock_guard guard(_mutex);
    _require_subscribed(variable, _rank);

    if (_variables[variable] == expected)
      update(variable, value);
  }

  void subscribe(std::string variable) {
    std::lock_guard guard(_mutex);
    _subscriptions[variable].insert(_rank);

    _send_to_all(Subscribe{variable, _rank});
  }

  void close() {
    _send_to_all(Close{});
  }

  void sync_msg(Msg msg) {
    std::lock_guard guard(_mutex);

    match(msg,
      [&](Close) {},
      [&](Subscribe m) { _subscriptions[m.variable].insert(m.process); },
      [&](Update m) { _variables[m.variable] = m.value; }
    );
  }
};

#endif
