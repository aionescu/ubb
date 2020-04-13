#include <fstream>
#include "FileRepo.hh"

std::vector<Task> FileRepo::_loadData() {
  std::vector<Task> tasks;
  std::ifstream inFile{_filePath};

  if (inFile.good())
    inFile >> tasks;
    
  return tasks;
}

void FileRepo::_saveData(const std::vector<Task>& tasks) {
  std::ofstream outFile{_filePath};
  outFile << tasks;
}

FileRepo::FileRepo(const std::string& filePath): _filePath{filePath} { }

const std::string& FileRepo::filePath() const {
  return _filePath;
}

bool FileRepo::add(const Task& newTask) {
  auto tasks = _loadData();
  auto isSuccesful = _add(tasks, newTask);

  if (isSuccesful)
    _saveData(tasks);

  return isSuccesful;
}

bool FileRepo::update(const Task& newTask) {
  auto tasks = _loadData();
  auto isSuccesful = _update(tasks, newTask);

  if (isSuccesful)
    _saveData(tasks);

  return isSuccesful;
}

bool FileRepo::remove(const std::string& title) {
  auto tasks = _loadData();
  auto isSuccesful = _remove(tasks, title);

  if (isSuccesful)
    _saveData(tasks);

  return isSuccesful;
}

std::vector<Task> FileRepo::data() {
  auto tasks = _loadData();
  return tasks;
}