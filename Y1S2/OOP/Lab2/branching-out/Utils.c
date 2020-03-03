#include <stdio.h>
#include <stdlib.h>
#include "Utils.h"

void failWith(const char* message) {
  printf("failWith: %s", message);
  exit(1);
}
