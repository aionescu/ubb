#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
  int day, month, year;
} date;

date parse_date(char* s) {
  s[2] = s[5] = '\0';

  int day = atoi(s);
  int month = atoi(s + 3);
  int year = atoi(s + 6);

  s[2] = s[5] = '-';

  date d = { day, month, year };
  return d;
}

int get_day_no(date d) {
  int days_in_month[13] = { 0, 31, (d.year % 4 == 0 ? 29 : 28), 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };

  int crr = 0;

  for (int i = 1; i < d.month; ++i) {
    crr += days_in_month[i];
  }

  crr += d.day;

  return crr;
}

int main() {
  char s[100];

  while (scanf("%s", s) && strcmp(s, "exit")) {
    date d = parse_date(s);
    int day_no = get_day_no(d);

    printf("%d ", day_no);
  }
}