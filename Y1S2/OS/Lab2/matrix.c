#include <stdio.h>
#include <stdlib.h>

int main(int argc, char** argv) {
  FILE* f = fopen(argv[1], "r");

  int n, m;
  fscanf(f, "%d", &n);
  fscanf(f, "%d", &m);

  int** a = malloc(n * sizeof(int*));

  for (int i = 0; i < n; ++i) {
    a[i] = malloc(m * sizeof(int));

    for (int j = 0; j < m; ++j)
      fscanf(f, "%d", &a[i][j]);
  }

  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < m; ++j)
      printf("%d ", a[i][j]);

    printf("\n");
  }

  for (int i = 0; i < n; ++i)
    free(a[i]);

  free(a);

  fclose(f);
}
