#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <pthread.h>

int has_digit(int n, int d) {
  if (!n)
    return d == 0;

  while (n) {
    if (n % 10 == d)
      return 1;

    n /= 10;
  }

  return 0;
}

int nums[55];
int crr = 0;
int global = 1;
pthread_t ts[10];
pthread_mutex_t mx;
pthread_barrier_t brr;

void* go(void* _arg) {
  while (1) {
    pthread_mutex_lock(&mx);

    if (global >= 55) {
      pthread_mutex_unlock(&mx);
      break;
    }

    ++global;

    if (has_digit(global, 2) || global % 2 == 0)
      nums[crr++] = global;
    else
      printf("%d\n", global);

    pthread_mutex_unlock(&mx);
  }

  pthread_barrier_wait(&brr);
  return NULL;
}

int main() {
  pthread_mutex_init(&mx, NULL);
  pthread_barrier_init(&brr, NULL, 11);

  int i = 0;
  for (i = 0; i < 10; ++i)
    pthread_create(&ts[i], NULL, go, NULL);

  pthread_barrier_wait(&brr);

  for (i = 0; i < 10; ++i)
    pthread_join(ts[i], NULL);

  for (i = 0; i < crr; ++i)
    printf("%d ", nums[i]);

  pthread_mutex_destroy(&mx);
  pthread_barrier_destroy(&brr);

  printf("\n");
  return 0;
}
