#include <regex>
#include "TaskValidator.hh"

InvalidTaskException::InvalidTaskException(const std::string& message) : _message{message} { }

const char* InvalidTaskException::what() const throw() {
  return _message.c_str();
}

void TaskValidator::validateTask(const Task& task) {
  if (task.title().empty())
    throw InvalidTaskException{"The task's title cannot be empty."};

  if (task.type().empty())
    throw InvalidTaskException{"The task's type cannot be empty."};

  std::regex datePattern{"^[0-9]{2}-[0-9]{2}-[0-9]{4}$"};
  if (!std::regex_match(task.lastPerformed(), datePattern))
    throw InvalidTaskException{"The task's last performed property must be a valid date (DD-MM-YYYY)."};

  if (task.timesPerformed() < 0)
    throw InvalidTaskException{"The task's times performed property must be a positive integer."};

  if (task.vision().empty())
    throw InvalidTaskException{"The task's vision cannot be empty."};
}