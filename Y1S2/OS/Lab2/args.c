#include <stdio.h>

int main(int argc, char** argv) {
  FILE* f = fopen(argv[1], "r");

  int n;
  fscanf(f, "%d", &n);

  printf("%d", n);
  fclose(f);
}
