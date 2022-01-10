#ifndef MSG_HH
#define MSG_HH

#include <iostream>

enum MsgTag {
  CLOSE,
  SUBSCRIBE,
  UPDATE
};

struct Msg {
  int tag;
  char var;
  int val;
};

std::ostream &operator <<(std::ostream &os, Msg msg) {
  switch (msg.tag) {
    case CLOSE:
      return os << "Close()";
    case SUBSCRIBE:
      return os << "Subscribe(" << msg.var << ", " << msg.val << ")";
    case UPDATE:
      return os << "Update(" << msg.var << ", " << msg.val << ")";
    default:
      throw std::runtime_error{"Invalid message tag"};
  }
}

#endif
