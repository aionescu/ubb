#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
  int day, month, year;
} Date;

Date parse_date(const char* string) {
  // String format is DD-MM-YYYY
  int day = atoi(string); // We read the day, which is at the beginning of the string
  int month = atoi(string + 3); // We need to read the month, so we skip 3 characters
  int year = atoi(string + 6); // We need to read the year, so we need to skip another 3 characters

  Date date = { day, month, year };
  return date;
}

int get_day_of_year(Date date) {
  // Array of days in each month of the year
  // Intended to be 1-indexed for easier index arithmetic
  int days_in_month[13] = { 0, 31, (date.year % 4 == 0 ? 29 : 28), 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };

  int day_of_year = date.day;

  for (int i = 1; i < date.month; ++i) {
    day_of_year += days_in_month[i];
  }

  return day_of_year;
}

int main() {
  char input[100];

  while (scanf("%s", input) && strcmp(input, "exit")) {
    Date date = parse_date(input);
    int day_of_year = get_day_of_year(date);

    printf("%d ", day_of_year);
  }
}