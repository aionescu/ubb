// Write a program that creates 7 threads and a global array of 10 integer 
// numbers, named SUM, storing the sum of numbers based on the remainder from 
// division with 10. Each thread generates random numbers between 0 and 100,
//  and adds each number n to SUM[x], where x is n mod 10. The threads stop when
//  there have been exactly 5 numbers generated with the last digit 5. 
// The main program prints the sums from the array SUM for each digit 
// (print SUM[x]=nr  for each x=0..9, where nr is the sum of all numbers 
// with remainder x from division by 10). Use efficient synchronisation 
// mechanisms and print also the numbers that make up the sums.

// Example:

// ….
// SUM[2]=12+22=34
// SUM[3]=93=93
// SUM[4]=84+24+64= 172
// ….


// ---
// "Focus on your goals, not on the obstacles."
// ---

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <pthread.h>
#include <time.h>

#define THREAD_COUNT 7

typedef struct Node {
  int val;
  struct Node* next;
} Node;

int length(Node* node) {
  int l = 0;

  while (node) {
    l++;
    node = node->next;
  }

  return l;
}

Node* nums[10];
int sum[10];
pthread_mutex_t ms[10];

void* go(void* _arg) {
  while (1) {
    int n = rand() % 100;
    int x = n % 10;

    pthread_mutex_lock(&ms[5]);

    int done = 0;
    if (length(nums[5]) >= 5)
      done = 1;
    
    pthread_mutex_unlock(&ms[5]);

    if (done)
      break;

    pthread_mutex_lock(&ms[x]);

    sum[x] += n;
    Node* new = malloc(sizeof(Node));
    new->val = n;
    new->next = nums[x];
    nums[x] = new;

    pthread_mutex_unlock(&ms[x]);  
  }

  return NULL;
}

int main() {
  srand(time(0));

  for (int i = 0; i < 10; ++i)
    pthread_mutex_init(&ms[i], NULL);

  pthread_t ts[THREAD_COUNT];

  int i;
  for (i = 0; i < THREAD_COUNT; ++i)
    pthread_create(&ts[i], NULL, go, NULL);

  for (i = 0; i < THREAD_COUNT; ++i)
    pthread_join(ts[i], NULL);
  
  for (i = 0; i < 10; ++i) {
    printf("sum[%d] = ", i);

    Node* node = nums[i];

    if (!node) {
      printf("0\n");
      continue;
    }

    printf("%d ", node->val);
    node = node->next;

    while (node) {
      printf("+ %d ", node->val);
      node = node->next;
    }

    printf("= %d\n", sum[i]);
  }

  for (i = 0; i < 10; ++i)
    pthread_mutex_destroy(&ms[i]);

  for (i = 0; i < 10; ++i) {
    Node* node = nums[i];

    while (node) {
      Node* oldNode = node;
      node = node->next;
      free(oldNode);
    }
  }

  return 0;
}
