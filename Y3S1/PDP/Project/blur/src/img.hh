#ifndef IMG_HH
#define IMG_HH

#include <cstdint>
#include "../stb/stb_image.h"
#include "../stb/stb_image_write.h"

using byte = std::uint8_t;

#define CHANNELS 3

struct Img {
  int width;
  int height;
  byte *pixels;
};

Img load_img(const char *path) {
  int width, height, channels;
  auto pixels = stbi_load(path, &width, &height, &channels, CHANNELS);

  if (!pixels) {
    printf("Error while loading the image\n");
    exit(1);
  }

  return {width, height, pixels};
}

void write_img(Img img, const char *path) {
  auto [width, height, pixels] = img;
  stbi_write_png(path, width, height, CHANNELS, pixels, width * CHANNELS);
}

void free_img(Img img) {
  stbi_image_free(img.pixels);
}

#endif
