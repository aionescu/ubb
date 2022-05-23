#!/bin/sh
set -euo pipefail

l() {
  pkgconf --libs --cflags $@
}

g++ \
  $(l glfw3) $(l freeglut) $(l glew) $(l assimp) \
  -I ../include ../include/glad.c \
  src/*.cc

./a.out
rm -f a.out
