#ifndef DOMAIN_HH
#define DOMAIN_HH

#include <string>

class Date {
  int day, month, year;

public:
  int getDay() const {
    return day;
  }

  int getMonth() const {
    return month;
  }

  int getYear() const {
    return year;
  }
};

class Task {
  std::string title, type, vision;
  Date lastPerformed;
  int timesPerformed;

public:
  std::string getTitle() const {
    return title;
  }

  std::string getType() const {
    return type;
  }

  Date getLastPerformed() const {
    return lastPerformed;
  }

  int getTimesPerformed() const {
    return timesPerformed;
  }

  std::string getVision() const {
    return vision;
  }
};

#endif