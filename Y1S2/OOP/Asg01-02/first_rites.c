#include <stdio.h>
#include <string.h>

int main() {
  char s[100];

  while (scanf("%s", s)) {
    if (!strcmp("exit", s))
      return 0;

    printf("%s", s);
  }
}