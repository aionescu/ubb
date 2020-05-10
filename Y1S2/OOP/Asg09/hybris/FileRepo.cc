#include "FileRepo.hh"

FileRepo::FileRepo(const std::string& filePath): _filePath{filePath} { }

const std::string& FileRepo::filePath() const {
  return _filePath;
}

void FileRepo::add(const Task& newTask) {
  auto tasks = _loadData();
  _add(tasks, newTask);

  _saveData(tasks);
}

void FileRepo::update(const Task& newTask) {
  auto tasks = _loadData();
  _update(tasks, newTask);

  _saveData(tasks);
}

void FileRepo::remove(const std::string& title) {
  auto tasks = _loadData();
  _remove(tasks, title);

  _saveData(tasks);
}

std::vector<Task> FileRepo::data() {
  auto tasks = _loadData();
  return tasks;
}