#ifndef MSG_HH
#define MSG_HH

#include <iostream>
#include <mpi.h>

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

void mpi_send_msg(Msg msg, int dest, int tag) {
  MPI_Bsend(&msg, sizeof(Msg), MPI_BYTE, dest, tag, MPI_COMM_WORLD);
}

Msg mpi_recv_msg(int source, int tag) {
  Msg msg;
  MPI_Recv(&msg, sizeof(Msg), MPI_BYTE, source, tag, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
  return msg;
}

#endif
