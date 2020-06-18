#include <algorithm>
#include <iostream>
#include <string>
#include <vector>

class Activity {
  std::string _description, _time;

public:
  Activity(std::string description, std::string time) : _description{description}, _time{time} { }

  bool operator <(const Activity& rhs) const {
    return _time < rhs._time;
  }

  friend std::ostream& operator <<(std::ostream& stream, const Activity& activity);
};

std::ostream& operator <<(std::ostream& stream, const Activity& activity) {
  return
    stream
    << "Activity " << activity._description
    << " will take place at " << activity._time << '.';
}

template <typename T>
class ToDo : public std::vector<T> {
public:
  ToDo& operator +=(const T& value) {
    this->push_back(value);
    std::sort(this->begin(), this->end());

    return *this;
  }

  // Prints all the elements of the ToDo list, in reverse order.
  void reversePrint(std::ostream& stream) {
    for (auto it = this->rbegin(); it != this->rend(); ++it)
      stream << *it << '\n';
  }
};

void ToDoList() {
  ToDo<Activity> todo{};
  Activity tiff{ "go to TIFF movie", "20:00" };
  todo += tiff;
  Activity project{ "present project assignment", "09.20" };
  todo += project;
  
  // iterates through the activities and prints them as follows:
  // Activity present project assignment will take place at 09.20.
  // Activity go to TIFF movie will take place at 20.00.
  for (auto a : todo)				
    std::cout << a << '\n';

  // Prints the activities as follows:
  // Activity go to TIFF movie will take place at 20.00.
  // Activity present project assignment will take place at 09.20.
  todo.reversePrint(std::cout);
}

int main() {
  ToDoList();
}