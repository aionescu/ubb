#ifndef BLUR_MPI_HH
#define BLUR_MPI_HH

#include <algorithm>
#include <iostream>
#include <cstdint>
#include <mpi.h>
#include "img.hh"

void blur_mpi_worker(int nproc, int rank, int radius) {
  Img img;

  MPI_Bcast(&img.width, 1, MPI_INT, 0, MPI_COMM_WORLD);
  MPI_Bcast(&img.height, 1, MPI_INT, 0, MPI_COMM_WORLD);

  img.pixels = new std::uint8_t[img.width * img.height * CHANNELS];
  MPI_Bcast(img.pixels, img.width * img.height * CHANNELS, MPI_BYTE, 0, MPI_COMM_WORLD);

  auto [width, height, pixels] = img;

  auto base_lines = height / nproc;
  auto extra_lines = height % nproc;

  auto lines_me = base_lines + (rank < extra_lines);
  auto first_line = rank * base_lines + std::min(rank, extra_lines);
  auto last_line = first_line + lines_me;

  auto diameter = radius * 2 + 1;
  auto area = diameter * diameter;

  auto buffer = new uint8_t[lines_me * width * CHANNELS];

  for (int y = first_line; y < last_line; ++y) {
    for (int x = 0; x < width; ++x) {
      int r = 0, g = 0, b = 0;

      for (int i = -radius; i <= radius; ++i) {
        for (int j = -radius; j <= radius; ++j) {
          int xi = x + i;
          int yj = y + j;

          if (xi < 0 || xi >= width || yj < 0 || yj >= height)
            continue;

          int index = (yj * width + xi) * 3;
          r += pixels[index + 0];
          g += pixels[index + 1];
          b += pixels[index + 2];
        }
      }

      int index = ((y - first_line) * width + x) * 3;
      buffer[index + 0] = r / area;
      buffer[index + 1] = g / area;
      buffer[index + 2] = b / area;
    }
  }

  MPI_Ssend(buffer, lines_me * width * CHANNELS, MPI_BYTE, 0, 0, MPI_COMM_WORLD);
}

void blur_mpi_master(int nproc, const char *in_path, const char *out_path, int radius) {
  auto img = load_img(in_path);
  auto [width, height, pixels] = img;

  MPI_Bcast(&width, 1, MPI_INT, 0, MPI_COMM_WORLD);
  MPI_Bcast(&height, 1, MPI_INT, 0, MPI_COMM_WORLD);
  MPI_Bcast(pixels, width * height * CHANNELS, MPI_BYTE, 0, MPI_COMM_WORLD);

  auto base_lines = height / nproc;
  auto extra_lines = height % nproc;

  auto lines_me = base_lines + (0 < extra_lines);

  auto diameter = radius * 2 + 1;
  auto area = diameter * diameter;

  Img new_img{width, height, new uint8_t[width * height * CHANNELS]};

  for (int y = 0; y < lines_me; ++y) {
    for (int x = 0; x < width; ++x) {
      int r = 0, g = 0, b = 0;

      for (int i = -radius; i <= radius; ++i) {
        for (int j = -radius; j <= radius; ++j) {
          int xi = x + i;
          int yj = y + j;

          if (xi < 0 || xi >= width || yj < 0 || yj >= height)
            continue;

          int index = (yj * width + xi) * 3;
          r += pixels[index + 0];
          g += pixels[index + 1];
          b += pixels[index + 2];
        }
      }

      int index = (y * width + x) * 3;
      new_img.pixels[index + 0] = r / area;
      new_img.pixels[index + 1] = g / area;
      new_img.pixels[index + 2] = b / area;
    }
  }

  for (int i = 1; i < nproc; ++i) {
    auto lines_i = base_lines + (i < extra_lines);
    auto first_line = i * base_lines + std::min(i, extra_lines);

    MPI_Recv(new_img.pixels + (first_line * width * CHANNELS), lines_i * width * CHANNELS, MPI_BYTE, i, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
  }

  write_img(out_path, new_img);
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
