#ifndef BLUR_MPI_HH
#define BLUR_MPI_HH

#include <algorithm>
#include <mpi.h>
#include "img.hh"

void blur_mpi_worker(int nproc, int rank, int radius) {
  int width, height;
  MPI_Bcast(&width, 1, MPI_INT, 0, MPI_COMM_WORLD);
  MPI_Bcast(&height, 1, MPI_INT, 0, MPI_COMM_WORLD);

  auto src_pixels = new byte[width * height * CHANNELS];
  MPI_Bcast(src_pixels, width * height * CHANNELS, MPI_BYTE, 0, MPI_COMM_WORLD);

  auto base_lines = height / nproc;
  auto extra_lines = height % nproc;

  auto lines_me = base_lines + (rank < extra_lines);
  auto first_line = rank * base_lines + std::min(rank, extra_lines);
  auto last_line = first_line + lines_me;

  auto diameter = radius * 2 + 1;
  auto area = diameter * diameter;

  auto dest_pixels = new byte[lines_me * width * CHANNELS];

  for (int y = first_line; y < last_line; ++y) {
    for (int x = 0; x < width; ++x) {
      int r = 0, g = 0, b = 0;

      auto up_bound = std::max(0, y - radius);
      auto down_bound = std::min(height - 1, y + radius);
      auto left_bound = std::max(0, x - radius);
      auto right_bound = std::min(width - 1, x + radius);

      for (int yj = up_bound; yj <= down_bound; ++yj)
        for (int xi = left_bound; xi <= right_bound; ++xi) {
          int index = (yj * width + xi) * CHANNELS;
          r += src_pixels[index];
          g += src_pixels[index + 1];
          b += src_pixels[index + 2];
        }

      int index = ((y - first_line) * width + x) * CHANNELS;
      dest_pixels[index] = r / area;
      dest_pixels[index + 1] = g / area;
      dest_pixels[index + 2] = b / area;
    }
  }

  MPI_Ssend(dest_pixels, lines_me * width * CHANNELS, MPI_BYTE, 0, 0, MPI_COMM_WORLD);

  delete[] dest_pixels;
  delete[] src_pixels;
}

void blur_mpi_master(int nproc, const char *in_path, const char *out_path, int radius) {
  auto [width, height, src_pixels] = load_img(in_path);

  MPI_Bcast(&width, 1, MPI_INT, 0, MPI_COMM_WORLD);
  MPI_Bcast(&height, 1, MPI_INT, 0, MPI_COMM_WORLD);
  MPI_Bcast(src_pixels, width * height * CHANNELS, MPI_BYTE, 0, MPI_COMM_WORLD);

  auto base_lines = height / nproc;
  auto extra_lines = height % nproc;

  auto lines_me = base_lines + (0 < extra_lines);

  auto diameter = radius * 2 + 1;
  auto area = diameter * diameter;

  auto dest_pixels = new byte[width * height * CHANNELS];

  for (int y = 0; y < lines_me; ++y) {
    for (int x = 0; x < width; ++x) {
      int r = 0, g = 0, b = 0;

      auto up_bound = std::max(0, y - radius);
      auto down_bound = std::min(height - 1, y + radius);
      auto left_bound = std::max(0, x - radius);
      auto right_bound = std::min(width - 1, x + radius);

      for (int yj = up_bound; yj <= down_bound; ++yj)
        for (int xi = left_bound; xi <= right_bound; ++xi) {
          int index = (yj * width + xi) * CHANNELS;
          r += src_pixels[index];
          g += src_pixels[index + 1];
          b += src_pixels[index + 2];
        }

      int index = (y * width + x) * CHANNELS;
      dest_pixels[index + 0] = r / area;
      dest_pixels[index + 1] = g / area;
      dest_pixels[index + 2] = b / area;
    }
  }

  for (int i = 1; i < nproc; ++i) {
    auto lines_i = base_lines + (i < extra_lines);
    auto first_line = i * base_lines + std::min(i, extra_lines);

    MPI_Recv(dest_pixels + (first_line * width * CHANNELS), lines_i * width * CHANNELS, MPI_BYTE, i, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
  }

  write_img({width, height, dest_pixels}, out_path);
}

void blur(const char *in_path, const char *out_path, int radius) {
  int nproc, rank;
  MPI_Init(0, 0);
  MPI_Comm_size(MPI_COMM_WORLD, &nproc);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);

  if (rank == 0)
    blur_mpi_master(nproc, in_path, out_path, radius);
  else
    blur_mpi_worker(nproc, rank, radius);

  MPI_Finalize();
}

#endif
