#include <stdio.h>

int main() {
  FILE* f = fopen("in.txt", "r");

  int n;
  fscanf(f, "%d", &n);

  printf("%d", n);
  fclose(f);
}
