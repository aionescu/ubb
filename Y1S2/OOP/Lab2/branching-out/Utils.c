#include <stdio.h>
#include <stdlib.h>
#include "Utils.h"

void failWith(Str message) {
  printf("failWith: %s", message);
  exit(1);
}
