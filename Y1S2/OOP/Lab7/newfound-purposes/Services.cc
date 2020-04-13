#include <algorithm>
#include <iterator>
#include "Services.hh"

void Services::_ensureMode(const std::string& mode) const {
  if (_mode != mode)
    throw WrongModeException{};
}

const std::string& Services::mode() const {
  return _mode;
}

void Services::setMode(const std::string& mode) {
  _mode = mode;
}

const std::string& Services::filePath() const {
  return _allTasks.filePath();
}

void Services::setFilePath(const std::string filePath) {
  _allTasks = FileRepo{filePath};
}

bool Services::add(const Task& newTask) {
  _ensureMode("A");

  return _allTasks.add(newTask);
}

bool Services::update(const Task& task) {
  _ensureMode("A");

  _servantTasks.update(task);
  return _allTasks.update(task);
}

bool Services::remove(const std::string& title) {
  _ensureMode("A");

  _servantTasks.remove(title);
  return _allTasks.remove(title);
}

std::vector<Task> Services::allTasks() {
  _ensureMode("A");

  return _allTasks.data();
}

std::pair<bool, Task> Services::next() {
  _ensureMode("B");

  auto data = _allTasks.data();

  if (data.size() == 0)
    return {false, {}};

  ++_servantCurrentTaskIndex;

  if (_servantCurrentTaskIndex == (int)data.size())
    _servantCurrentTaskIndex = 0;

  return {true, data.at(_servantCurrentTaskIndex)};
}

bool Services::save(const std::string& title) {
  _ensureMode("B");

  auto tasks = _allTasks.data();

  auto predicate = [&](const Task& task) { return task.title() == title; };
  auto findResult = std::find_if(tasks.begin(), tasks.end(), predicate);

  if (findResult == tasks.end())
    return false;

  return _servantTasks.add(*findResult);
}

std::vector<Task> Services::tasksByTimesPerformed(const std::string& type, int maxTimesPerformed) {
  _ensureMode("B");

  std::vector<Task> result;
  auto data = _allTasks.data();

  auto predicate = [&](const Task& task) {
    return task.type() == type && task.timesPerformed() < maxTimesPerformed;
  };

  std::copy_if(data.begin(), data.end(), std::back_inserter(result), predicate);
  return result;
}

std::vector<Task> Services::servantTasks() {
  _ensureMode("B");

  return _servantTasks.data();
}