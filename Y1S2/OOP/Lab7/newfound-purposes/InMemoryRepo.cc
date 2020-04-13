#include "InMemoryRepo.hh"

bool InMemoryRepo::add(const Task& newTask) {
  return _add(_tasks, newTask);
}

// Attempts to update the specified task in this repository,
// if it exists.
bool InMemoryRepo::update(const Task& newTask) {
  return _update(_tasks, newTask);
}

// Attempts to remove the task with the specified title
// from this repository, if it exists.
bool InMemoryRepo::remove(const std::string& title) {
  return _remove(_tasks, title);
}

// Returns a copy of this repository's data.
std::vector<Task> InMemoryRepo::data() {
  return _tasks;
}