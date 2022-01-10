#include <vector>
#include "dsm.hh"

void listen(Dsm *dsm) {
  while (true) {
    auto msg = mpi_recv_msg(MPI_ANY_SOURCE, MPI_ANY_TAG);

    dsm->sync_msg(msg);
    std::cout << mpi_rank() << " > Received " << msg << std::endl;

    if (msg.tag == CLOSE)
      break;
  }
}

void actor_0(Dsm *dsm) {
  dsm->subscribe('a');
}

void actor_1(Dsm *dsm) {
  dsm->subscribe('a');
  dsm->update('a', 222);
}

void actor_2(Dsm *dsm) {
  dsm->subscribe('a');
  dsm->update('a', 333);
  dsm->compare_exchange('a', 333, 444);
  dsm->close();
}

std::vector<void(*)(Dsm *dsm)> actors{actor_0, actor_1, actor_2};

int main(int argc, char **argv) {
  MPI_Init(&argc, &argv);

  auto nproc = mpi_nproc();
  auto rank = mpi_rank();

  if (rank == 0 && (std::size_t)nproc != actors.size()) {
    std::cout << "nproc != actor count. Quitting.\n";
    MPI_Abort(MPI_COMM_WORLD, 1);
  }

  Dsm dsm;

  std::thread listener{listen, &dsm};
  std::thread actor{actors[rank], &dsm};

  actor.join();
  listener.join();

  std::cout << rank << "> Vars: " << dsm.vars() << ", Subs: " << dsm.subs() << std::endl;
  MPI_Finalize();
}
