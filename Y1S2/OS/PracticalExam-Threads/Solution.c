#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <pthread.h>
#include <string.h>

typedef struct {
  char* f;
  int n;
} Pair;

int global = 0;
pthread_mutex_t mutex;

void* go(void* arg) {
  Pair* pair = (Pair*)arg;

  FILE* file = fopen(pair->f, "r");
  if (!file) {
    printf("Can't open file \"%s\".\n", pair->f);
    return NULL;
  }

  int evens = 0;

  while (evens < pair->n) {
    int x;
    if (fscanf(file, "%d", &x) == EOF) {
      printf("File \"%s\" is too short.\n", pair->f);
      break;
    }

    if (x % 2 == 0) {
      ++evens;

      pthread_mutex_lock(&mutex);
      global += x;
      pthread_mutex_unlock(&mutex);
    }
  }

  fclose(file);
  return NULL;
}

int main(int argc, char** argv) {
  // Skip the first arg, which is the program's name.
  --argc;
  ++argv;

  if (argc % 2 != 0) {
    printf("Expecting even number of args.\n");
    return 1;
  }

  int pairCount = argc / 2;

  pthread_mutex_init(&mutex, NULL);
  pthread_t* threads = malloc(pairCount * sizeof(pthread_t));
  Pair* pairs = malloc(pairCount * sizeof(Pair));

  int i;
  for (i = 0; i < pairCount; ++i) {
    pairs[i].f = argv[i * 2];
    pairs[i].n = atoi(argv[i * 2 + 1]);

    pthread_create(&threads[i], NULL, go, (void*)&pairs[i]);
  }

  for (i = 0; i < pairCount; ++i)
    pthread_join(threads[i], NULL);

  printf("The global variable is %d.\n", global);

  free(pairs);
  free(threads);
  pthread_mutex_destroy(&mutex);

  return 0;
}
