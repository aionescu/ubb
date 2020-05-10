#include <algorithm>
#include <iterator>
#include "CSVRepo.hh"
#include "HTMLRepo.hh"
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

const std::string& Services::servantTasksFilePath() const {
  return _servantTasks->filePath();
}

void Services::setFilePath(const std::string& filePath) {
  _allTasks = CSVRepo{filePath};
}

void Services::setServantTasksFilePath(const std::string& filePath) {
  auto extension = filePath.substr(filePath.find_last_of(".") + 1);

  if (extension == "html")
    _servantTasks.reset(new HTMLRepo{filePath});
  else if (extension == "csv" || extension == "txt")
    _servantTasks.reset(new CSVRepo{filePath});
  else
    throw InvalidFileTypeException{};
}

void Services::add(const Task& newTask) {
  _ensureMode("A");
  _taskValidator.validateTask(newTask);

  try {
    _allTasks.add(newTask);
  } catch (InvalidRepoActionException&) {
    throw InvalidServicesActionException{};
  }
}

void Services::update(const Task& task) {
  _ensureMode("A");
  _taskValidator.validateTask(task);
  
  try {
    _servantTasks->update(task);
  } catch (InvalidRepoActionException&) { }

  try {
    _allTasks.update(task);
  } catch (InvalidRepoActionException& e) {
    throw InvalidServicesActionException{};
  }
}

void Services::remove(const std::string& title) {
  _ensureMode("A");

  try {
    _servantTasks->remove(title);
  } catch (InvalidRepoActionException&) { }

  try {
    _allTasks.remove(title);
  } catch (InvalidRepoActionException& e) {
    throw InvalidServicesActionException{};
  }
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

void Services::save(const std::string& title) {
  _ensureMode("B");

  auto tasks = _allTasks.data();

  auto predicate = [&](const Task& task) { return task.title() == title; };
  auto findResult = std::find_if(tasks.begin(), tasks.end(), predicate);

  if (findResult == tasks.end())
    throw InvalidServicesActionException{};

  try {
    _servantTasks->add(*findResult);
  } catch (InvalidRepoActionException& e) {
    throw InvalidServicesActionException{};
  }
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

  return _servantTasks->data();
}