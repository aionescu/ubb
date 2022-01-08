#include <iostream>

#ifdef USE_MPI
#include "blur_mpi.hpp"
#else
#include "blur_mt.hpp"
#endif

int main(int argc, char** argv) {
  if (argc < 4) {
    std::cout << "Usage: blur <input-path> <output-path> <blur-radius>\n";
    return 1;
  }

  auto in_path = argv[1];
  auto out_path = argv[2];
  auto radius = std::atoi(argv[3]);

  blur(in_path, out_path, radius);
}
