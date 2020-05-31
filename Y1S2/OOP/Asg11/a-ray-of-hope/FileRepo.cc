#include "FileRepo.hh"

FileRepo::FileRepo(const std::string& filePath): _filePath{filePath} { }

std::string FileRepo::filePath() const {
  return _filePath;
}

void FileRepo::add(const Task& newTask) {
  auto tasks = _loadData();
  _add(tasks, newTask);

  _saveData(tasks);
}

Task FileRepo::update(const Task& newTask) {
  auto tasks = _loadData();
  auto result = _update(tasks, newTask);

  _saveData(tasks);
  return result;
}

Task FileRepo::remove(const std::string& title) {
  auto tasks = _loadData();
  auto result = _remove(tasks, title);

  _saveData(tasks);
  return result;
}

std::vector<Task> FileRepo::data() {
  auto tasks = _loadData();
  return tasks;
}