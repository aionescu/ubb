#ifndef IMG_HH
#define IMG_HH

#include "../stb/stb_image.h"
#include "../stb/stb_image_write.h"

#define CHANNELS 3

struct Img {
  int width;
  int height;
  std::uint8_t *pixels;
};

Img load_img(const char *path) {
  int width, height, channels;
  auto pixels = stbi_load(path, &width, &height, &channels, CHANNELS);
  channels = CHANNELS;

  if (pixels == NULL) {
    printf("Error while loading the image\n");
    exit(1);
  }

  return {width, height, pixels};
}

void write_img(const char *path, Img img) {
  stbi_write_png(path, img.width, img.height, CHANNELS, img.pixels, img.width * CHANNELS);
}

#endif
