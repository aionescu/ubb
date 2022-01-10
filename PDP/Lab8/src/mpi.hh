#ifndef MPI_HH
#define MPI_HH

#include <mpi.h>
#include "msg.hh"

#define BCAST 0x3f3f3f3f

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

bool mpi_is_master() {
  return mpi_rank() == 0;
}

void mpi_send_int(int value, int dest, int tag) {
  if (tag == BCAST)
    MPI_Bcast(&value, 1, MPI_INT, dest, MPI_COMM_WORLD);
  else
    MPI_Bsend(&value, 1, MPI_INT, dest, tag, MPI_COMM_WORLD);
}

int mpi_recv_int(int source, int tag) {
  int value;

  if (tag == BCAST)
    MPI_Bcast(&value, 1, MPI_INT, source, MPI_COMM_WORLD);
  else
    MPI_Recv(&value, 1, MPI_INT, source, tag, MPI_COMM_WORLD, MPI_STATUS_IGNORE);

  return value;
}

void mpi_send_buffer(void *buffer, int length, MPI_Datatype type, int dest, int tag) {
  if (tag == BCAST)
    MPI_Bcast(buffer, length, type, dest, MPI_COMM_WORLD);
  else
    MPI_Bsend(buffer, length, type, dest, tag, MPI_COMM_WORLD);
}

void mpi_recv_buffer(void *buffer, int length, MPI_Datatype type, int source, int tag) {
  if (tag == BCAST)
    MPI_Bcast(buffer, length, type, source, MPI_COMM_WORLD);
  else
    MPI_Recv(buffer, length, type, source, tag, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
}

void mpi_send_string(std::string string, int dest, int tag) {
  mpi_send_int(string.size(), dest, tag);
  mpi_send_buffer(string.data(), string.size(), MPI_CHAR, dest, tag);
}

std::string mpi_recv_string(int source, int tag) {
  auto length = mpi_recv_int(source, tag);
  std::string string(length, '\0');

  mpi_recv_buffer(string.data(), length, MPI_CHAR, source, tag);
  return string;
}

void mpi_send_msg(Msg msg, int dest, int tag) {
  mpi_send_int(msg_tag(msg), dest, tag);

  match(msg,
    [=](Close) { },
    [=](Subscribe a) {
      mpi_send_string(a.variable, dest, tag);
      mpi_send_int(a.process, dest, tag);
    },
    [=](Update a) {
      mpi_send_string(a.variable, dest, tag);
      mpi_send_int(a.value, dest, tag);
    }
  );
}

Msg mpi_recv_msg(int source, int tag) {
  switch (mpi_recv_int(source, tag)) {
    case 0:
      return Close{};
    case 1: {
      return Subscribe{
        mpi_recv_string(source, tag),
        mpi_recv_int(source, tag)
      };
    }
    case 2: {
      return Update{
        mpi_recv_string(source, tag),
        mpi_recv_int(source, tag)
      };
    }
    default:
      throw std::runtime_error{"Invalid message tag"};
  }
}

#endif
