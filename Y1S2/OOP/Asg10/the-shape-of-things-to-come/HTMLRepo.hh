#ifndef HTML_REPO_HH
#define HTML_REPO_HH

#include <string>
#include <vector>
#include "Domain.hh"
#include "FileRepo.hh"

class HTMLRepo: public FileRepo {
protected:
  std::vector<Task> _loadData() override;
  void _saveData(const std::vector<Task>& tasks) override;

public:
  HTMLRepo(const std::string& filePath);
};

#endif