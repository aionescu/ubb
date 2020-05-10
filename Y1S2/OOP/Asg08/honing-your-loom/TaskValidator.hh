#ifndef TASK_VALIDATOR_HH
#define TASK_VALIDATOR_HH

#include <exception>
#include <string>
#include "Domain.hh"

class InvalidTaskException : std::exception {
private:
  std::string _message;

public:
  InvalidTaskException(const std::string& message = "Invalid task.");

  const char* what() const throw();
};

class TaskValidator {
public:
  void validateTask(const Task& task);
};

#endif