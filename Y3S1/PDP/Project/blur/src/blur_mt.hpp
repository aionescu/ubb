#ifndef BLUR_MT_HPP
#define BLUR_MT_HPP

#include <algorithm>
#include <iostream>
#include <cstdint>
#include <cstdlib>
#include <unistd.h>
#include <pthread.h>
#include <thread>
#include "img.hpp"

struct ThreadData {
  int id;
  int nproc;
  int radius;
  Img source;
  Img dest;
};

void *blur_mt_worker(void *arg) {
  auto [id, nproc, radius, source, dest] = *(ThreadData *)arg;
  auto [width, height, pixels] = source;

  auto base_lines = height / nproc;
  auto extra_lines = height % nproc;

  auto lines_me = base_lines + (id < extra_lines);
  auto first_line = id * base_lines + std::min(id, extra_lines);
  auto last_line = first_line + lines_me;

  auto diameter = radius * 2 + 1;
  auto area = diameter * diameter;

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

      int index = (y * width + x) * 3;
      dest.pixels[index + 0] = r / area;
      dest.pixels[index + 1] = g / area;
      dest.pixels[index + 2] = b / area;
    }
  }

  return NULL;
}

Img blur_mt(Img img, int radius, int nproc) {
  auto [width, height, pixels] = img;
  auto dest = Img{width, height, new std::uint8_t[width * height * 3]};

  auto data = new ThreadData[nproc];
  for (int i = 0; i < nproc; ++i)
    data[i] = {i, nproc, radius, img, dest};

  pthread_t threads[nproc];
  for (int i = 0; i < nproc; ++i)
    pthread_create(&threads[i], NULL, blur_mt_worker, &data[i]);

  for (int i = 0; i < nproc; ++i)
    pthread_join(threads[i], NULL);

  delete[] data;
  return dest;
}

void blur(const char *in_path, const char *out_path, int radius) {
  auto nproc = std::thread::hardware_concurrency();
  auto img = load_img(in_path);

  auto new_img = blur_mt(img, radius, nproc);
  write_img(out_path, new_img);
}

#endif
