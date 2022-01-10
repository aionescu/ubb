#include "dsm.hh"

void listen(Dsm *dsm) {
  while (true) {
    auto msg = mpi_recv_msg(MPI_ANY_SOURCE, MPI_ANY_TAG);

    dsm->sync_msg(msg);
    std::cout << mpi_rank() << " > Received " << msg << std::endl;

    if (std::holds_alternative<Close>(msg))
      break;
  }
}

void actor_0(Dsm *dsm) {
  dsm->subscribe("a");
}

void actor_1(Dsm *dsm) {
  dsm->subscribe("a");
  dsm->update("a", 222);
  dsm->close();
}

std::vector<void(*)(Dsm *dsm)> actors{actor_0, actor_1};

int main(int argc, char **argv) {
  MPI_Init(&argc, &argv);

  auto rank = mpi_rank();

  if ((std::size_t)rank < actors.size()) {
    Dsm dsm;

    std::thread listener{listen, &dsm};
    std::thread actor{actors[rank], &dsm};

    actor.join();
    listener.join();
  }

  std::cout << mpi_rank() << " > Closed" << std::endl;
  MPI_Finalize();
}
