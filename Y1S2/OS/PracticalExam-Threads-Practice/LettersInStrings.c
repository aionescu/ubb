#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <pthread.h>

int get_app(char* s, char c) {
  int val = 0;

  while (*s)
    if (*(s++) == c)
      ++val;

  return val;
}

char** wl[5];
int app[5], words[5];
pthread_mutex_t mx[5];

void* go(void* arg) {
  char* s = (char*)arg;
  int i = 0;

  for (i = 0; i < 5; ++i) {
    int num_app = get_app(s, (char)(i + 'a'));

    if (num_app > 0) {
      pthread_mutex_lock(&mx[i]);

      app[i] += num_app;
      wl[i][words[i]++] = s;

      pthread_mutex_unlock(&mx[i]);
    }
  }

  return NULL;
}

int main(int argc, char** argv) {
  --argc;
  ++argv;

  int i = 0;
  pthread_t* ts = malloc(argc * sizeof(pthread_t));

  for (i = 0; i < 5; ++i) {
    wl[i] = malloc(argc * sizeof(char*));
    pthread_mutex_init(&mx[i], NULL);
  }

  for (i = 0; i < argc; ++i)
    pthread_create(&ts[i], NULL, go, (void*)argv[i]);

  for (i = 0; i < argc; ++i)
    pthread_join(ts[i], NULL);

  for (i = 0; i < 5; ++i) {
    printf("%c: apparitions %d, words %d => ", (char)(i + 'a'), app[i], words[i]);

    if (words[i]) {
      printf("%s", wl[i][0]);

      int j = 0;
      for (j = 1; j < words[i]; ++j)
        printf(", %s", wl[i][j]);
    }

    printf("\n");
  }  

  free(ts);

  for (i = 0; i < 5; ++i) {
    free(wl[i]);
    pthread_mutex_destroy(&mx[i]);
  }

  return 0;
}
