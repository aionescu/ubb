#include <algorithm>
#include <iterator>
#include "InMemoryRepo.hh"
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

std::string Services::filePath() const {
  return _allTasks->filePath();
}

std::string Services::servantTasksFilePath() const {
  return _servantTasks->filePath();
}

Repo* _getRepoByFilePath(const std::string& filePath) {
  if (filePath.size() == 0)
    return new InMemoryRepo;

  auto extension = filePath.substr(filePath.find_last_of(".") + 1);

  if (extension == "html")
    return new HTMLRepo{filePath};
  else if (extension == "csv" || extension == "txt")
    return new CSVRepo{filePath};
  else
    throw InvalidFileTypeException{};
}

void Services::setFilePath(const std::string& filePath) {
  _allTasks.reset(_getRepoByFilePath(filePath));
}

void Services::setServantTasksFilePath(const std::string& filePath) {
  _servantTasks.reset(_getRepoByFilePath(filePath));
}

void Services::add(const Task& newTask) {
  _ensureMode("A");
  _taskValidator.validateTask(newTask);

  try {
    _allTasks->add(newTask);

    std::unique_ptr<Action> action = std::make_unique<AddAction>(*_allTasks, newTask);
    _done.push(std::move(action));
    _undone = {};
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
    auto oldTask = _allTasks->update(task);

    std::unique_ptr<Action> action = std::make_unique<UpdateAction>(*_allTasks, oldTask, task);
    _done.push(std::move(action));
    _undone = {};
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
    auto removedTask = _allTasks->remove(title);

    std::unique_ptr<Action> action = std::make_unique<RemoveAction>(*_allTasks, removedTask);
    _done.push(std::move(action));
    _undone = {};
  } catch (InvalidRepoActionException& e) {
    throw InvalidServicesActionException{};
  }
}

std::vector<Task> Services::allTasks() {
  _ensureMode("A");

  return _allTasks->data();
}

void Services::resetNext() {
  _servantCurrentTaskIndex = -1;
}

std::pair<bool, Task> Services::next() {
  _ensureMode("B");

  auto data = _allTasks->data();

  if (data.size() == 0)
    return {false, {}};

  ++_servantCurrentTaskIndex;

  if (_servantCurrentTaskIndex >= (int)data.size())
    _servantCurrentTaskIndex = 0;

  return {true, data.at(_servantCurrentTaskIndex)};
}

void Services::save(const std::string& title) {
  _ensureMode("B");

  auto tasks = _allTasks->data();

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
  auto data = _allTasks->data();

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

std::vector<Task> Services::mylist() {
  return _servantTasks->data();
}

void Services::undo() {
  if (_done.empty())
    throw InvalidUndoException{};

  auto action = std::move(_done.top());
  _done.pop();

  action->undo();
  _undone.push(std::move(action));
}

void Services::redo() {
  if (_undone.empty())
    throw InvalidRedoException{};

  auto action = std::move(_undone.top());
  _undone.pop();

  action->redo();
  _done.push(std::move(action));

}