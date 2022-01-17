#ifndef BLUR_MT_HH
#define BLUR_MT_HH

#include <algorithm>
#include <thread>
#include "img.hh"

struct ThreadData {
  int id;
  int nproc;
  int radius;
  int width;
  int height;
  byte *src_pixels;
  byte *dest_pixels;
};

void *blur_mt_worker(void *arg) {
  auto [id, nproc, radius, width, height, src_pixels, dest_pixels] = *(ThreadData *)arg;

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
      dest_pixels[index] = r / area;
      dest_pixels[index + 1] = g / area;
      dest_pixels[index + 2] = b / area;
    }
  }

  return nullptr;
}

Img blur_mt(Img img, int radius, int nproc) {
  auto [width, height, src_pixels] = img;
  auto dest_pixels = new byte[width * height * CHANNELS];

  auto data = new ThreadData[nproc];
  for (int i = 0; i < nproc; ++i)
    data[i] = {i, nproc, radius, width, height, src_pixels, dest_pixels};

  auto threads = new pthread_t[nproc];
  for (int i = 0; i < nproc; ++i)
    pthread_create(&threads[i], nullptr, blur_mt_worker, &data[i]);

  for (int i = 0; i < nproc; ++i)
    pthread_join(threads[i], nullptr);

  delete[] threads;
  delete[] data;

  return {width, height, dest_pixels};
}

void blur(const char *in_path, const char *out_path, int radius) {
  auto nproc = std::thread::hardware_concurrency();
  auto img = load_img(in_path);

  auto new_img = blur_mt(img, radius, nproc);
  write_img(new_img, out_path);

  delete[] new_img.pixels;
  free_img(img);
}

#endif
