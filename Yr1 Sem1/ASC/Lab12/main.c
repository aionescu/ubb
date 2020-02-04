#include <stdio.h>

int commonPrefixLength(const char* a, const char* b);

char a[] = "Stringa";
char b[] = "Stringb";
char c[] = "Strc";

int main() {
  int p = commonPrefixLength(a, b);
  printf("%.*s\n", p, a);

  p = commonPrefixLength(a, c);
  printf("%.*s\n", p, a);

  p = commonPrefixLength(b, c);
  printf("%.*s\n", p, b);
  
  return 0;
}