#ifndef DSM_HH
#define DSM_HH

#include <map>
#include <set>
#include <mutex>
#include "msg.hh"

int mpi_nproc() {
  int nproc;
  MPI_Comm_size(MPI_COMM_WORLD, &nproc);
  return nproc;
}

int mpi_rank() {
  int rank;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  return rank;
}

template <typename T>
std::ostream &operator <<(std::ostream &os, const std::set<T> &set) {
  os << "[";

  for (auto it = set.begin(); it != set.end(); ++it) {
    if (it != set.begin())
      os << ", ";

    os << *it;
  }

  return os << "]";
}

template <typename K, typename V>
std::ostream &operator <<(std::ostream &os, const std::map<K, V> &map) {
  os << "{";

  for (auto it = map.begin(); it != map.end(); ++it) {
    if (it != map.begin())
      os << ", ";

    os << it->first << " = " << it->second;
  }

  return os << "}";
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

  std::map<char, std::set<int>> _subs;
  std::map<char, int> _vars;
  std::recursive_mutex _mutex;

  void _require_subscribed(char var, int process) {
    if (_subs[var].find(process) == _subs[var].end())
      throw DsmException{"Process " + std::to_string(process) + " is not subscribed to variable " + var};
  }

  void _send_to_subscribers(char var, Msg msg) {
    std::cout << _rank << " > Send: " << msg << " to " << _subs[var] << std::endl;

    for (int i = 0; i < _nproc; ++i)
      if (_rank != i && _subs[var].count(i))
        mpi_send_msg(msg, i, 0);
  }

  void _send_to_all(Msg msg) {
    std::cout << _rank << " > Send: " << msg << " to all" << std::endl;

    for (int i = 0; i < _nproc; ++i)
      if (_rank != i || msg.tag == CLOSE)
        mpi_send_msg(msg, i, 0);
  }

public:
  const std::map<char, std::set<int>> &subs() const {
    return _subs;
  }

  const std::map<char, int> &vars() const {
    return _vars;
  }

  void update(char var, int val) {
    std::lock_guard guard(_mutex);
    _require_subscribed(var, _rank);

    _vars[var] = val;
    _send_to_subscribers(var, {UPDATE, var, val});
  }

  void compare_exchange(char var, int expected, int val) {
    std::lock_guard guard(_mutex);
    _require_subscribed(var, _rank);

    if (_vars[var] == expected)
      update(var, val);
  }

  void subscribe(char var) {
    std::lock_guard guard(_mutex);
    _subs[var].insert(_rank);

    _send_to_all({SUBSCRIBE, var, _rank});
  }

  void close() {
    _send_to_all({CLOSE, 0, 0});
  }

  void sync_msg(Msg msg) {
    std::lock_guard guard(_mutex);

    switch (msg.tag) {
      case SUBSCRIBE:
        _subs[msg.var].insert(msg.val);
        break;

      case UPDATE:
        _require_subscribed(msg.var, msg.val);
        _vars[msg.var] = msg.val;
        break;

      default:
        break;
    }
  }
};

#endif
