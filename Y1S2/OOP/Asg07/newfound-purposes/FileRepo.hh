#ifndef FILE_REPO_HH
#define FILE_REPO_HH

#include <string>
#include <vector>
#include "Domain.hh"
#include "Repo.hh"

class FileRepo: public Repo {
  std::string _filePath;

  std::vector<Task> _loadData();

  void _saveData(const std::vector<Task>& tasks);

public:
  FileRepo(const std::string& filePath);

  const std::string& filePath() const;

  bool add(const Task& newTask) override;
  bool update(const Task& newTask) override;
  bool remove(const std::string& title) override;

  std::vector<Task> data() override;
};

#endif