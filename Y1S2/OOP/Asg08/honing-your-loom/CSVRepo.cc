#include <fstream>
#include "CSVRepo.hh"

std::vector<Task> CSVRepo::_loadData() {
  std::vector<Task> tasks;
  std::ifstream inFile{_filePath};

  if (inFile.good())
    inFile >> tasks;
    
  return tasks;
}

void CSVRepo::_saveData(const std::vector<Task>& tasks) {
  std::ofstream outFile{_filePath};
  outFile << tasks;
}

CSVRepo::CSVRepo(const std::string& filePath): FileRepo{filePath} { }