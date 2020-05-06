#ifndef CSV_REPO_HH
#define CSV_REPO_HH

#include <string>
#include <vector>
#include "Domain.hh"
#include "FileRepo.hh"

class CSVRepo: public FileRepo {
protected:
  std::vector<Task> _loadData() override;
  void _saveData(const std::vector<Task>& tasks) override;

public:
  CSVRepo(const std::string& filePath);
};

#endif