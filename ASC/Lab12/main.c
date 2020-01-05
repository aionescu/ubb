#include <stdio.h>

void printN(const char* s, int n);
int commonPrefixLength(const char* a, const char* b);

char a[] = "Stringa";
char b[] = "Stringb";
char c[] = "Strc";

int main()
{
  int p = commonPrefixLength(a, b);
  printN(a, p);

  p = commonPrefixLength(a, c);
  printN(a, p);

  p = commonPrefixLength(b, c);
  printN(b, p);
  
  return 0;
}