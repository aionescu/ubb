#include "Repo.hh"

bool Repo::_add(std::vector<Task>& tasks, const Task& newTask) {
  auto predicate = [&](const Task& task) { return task.title() == newTask.title(); };
  auto findResult = std::find_if(tasks.begin(), tasks.end(), predicate);

  if (findResult != tasks.end())
    return false;

  tasks.push_back(newTask);
  return true;
}

bool Repo::_update(std::vector<Task>& tasks, const Task& newTask) {
  auto predicate = [&](const Task& task) { return task.title() == newTask.title(); };
  auto findResult = std::find_if(tasks.begin(), tasks.end(), predicate);

  if (findResult == tasks.end())
    return false;

  *findResult = newTask;
  return true;
}

bool Repo::_remove(std::vector<Task>& tasks, const std::string& title) {
  auto predicate = [&](const Task& task) { return task.title() == title; };

  auto findResult = std::find_if(tasks.begin(), tasks.end(), predicate);

  if (findResult == tasks.end())
    return false;

  auto removeResult = std::remove_if(tasks.begin(), tasks.end(), predicate);
  tasks.erase(removeResult, tasks.end());

  return true;
}