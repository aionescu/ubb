#ifndef MSG_HH
#define MSG_HH

#include <functional>
#include <iomanip>
#include <iostream>
#include <variant>

template <typename... Ts>
struct Overloaded: Ts... {
  using Ts::operator()...;
};

template <typename... Ts>
Overloaded(Ts...) -> Overloaded<Ts...>;

struct Close {};

struct Subscribe {
  std::string variable;
  int process;
};

struct Update {
  std::string variable;
  int value;
};

using Msg = std::variant<Close, Subscribe, Update>;

template <typename... Fs>
auto match(Msg msg, Fs... fs) {
  return std::visit(Overloaded{fs...}, msg);
}

int msg_tag(Msg msg) {
  return match(msg,
    [](Close) { return 0; },
    [](Subscribe) { return 1; },
    [](Update) { return 2; }
  );
}

std::ostream &operator <<(std::ostream &os, Msg msg) {
  match(msg,
    [&](Close) { os << "Close{}"; },
    [&](Subscribe m) { os << "Subscribe{" << std::quoted(m.variable) << ", " << m.process << "}"; },
    [&](Update m) { os << "Update{" << std::quoted(m.variable) << ", " << m.value << "}"; }
  );

  return os;
}

#endif
