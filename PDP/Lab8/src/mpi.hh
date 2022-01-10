#ifndef MPI_HH
#define MPI_HH

#include <mpi.h>
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

void mpi_send_msg(Msg msg, int dest, int tag) {
  MPI_Bsend(&msg, sizeof(Msg), MPI_BYTE, dest, tag, MPI_COMM_WORLD);
}

Msg mpi_recv_msg(int source, int tag) {
  Msg msg;
  MPI_Recv(&msg, sizeof(Msg), MPI_BYTE, source, tag, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
  return msg;
}

#endif
