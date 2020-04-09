#include <cassert>
#include <cstdio>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>
#include "Domain.hh"
#include "Repo.hh"
#include "FileRepo.hh"
#include "Services.hh"

void test_Task_title_noInput_returnsTaskTitle() {
  Task task{"title", "type", "lastPerformed", 0, "vision"};

  assert(task.title() == "title");
}

void test_Task_type_noInput_returnsTaskType() {
  Task task{"title", "type", "lastPerformed", 0, "vision"};

  assert(task.type() == "type");
}

void test_Task_lastPerformed_noInput_returnsTaskLastPerformed() {
  Task task{"title", "type", "lastPerformed", 0, "vision"};

  assert(task.lastPerformed() == "lastPerformed");
}

void test_Task_timesPerformed_noInput_returnsTaskTimesPerformed() {
  Task task{"title", "type", "lastPerformed", 0, "vision"};

  assert(task.timesPerformed() == 0);
}

void test_Task_vision_noInput_returnsTaskVision() {
  Task task{"title", "type", "lastPerformed", 0, "vision"};

  assert(task.vision() == "vision");
}

void test_Task_equalsOperator_anyTask_taskIsEqualToItself() {
  Task task;
  assert(task == task);
}

void test_Domain_trimString_paddedString_stringIsTrimmed() {
  std::string string = "   abc   ";
  trimString(string);

  assert(string == "abc");
}

void test_Domain_splitString_stringWithMultipleTokens_allTokensAreSeparated() {
  std::string input = "abc, def, ghi, jkl";
  std::vector<std::string> expectedOutput{{"abc", "def", "ghi", "jkl"}};

  auto output = splitString(input, ',');
  assert(output == expectedOutput);
}

void test_Repo_ctorWithVector_anyVector_repoDataEqualsOriginalVector() {
  Task task;
  std::vector<Task> vector{{task, task, task}};

  Repo repo{vector};
  assert(repo.data() == vector);
}

void test_Repo_add_inexistentTask_operationSucceeds() {
  Repo repo;
  Task task;

  assert(repo.add(task));
}

void test_Repo_add_existingTask_operationFails() {
  Repo repo;
  Task task;

  repo.add(task);
  assert(!repo.add(task));
}

void test_Repo_update_existingTask_operationSucceeds() {
  Repo repo;
  Task task;

  repo.add(task);
  assert(repo.update(task));
}

void test_Repo_update_inexistentTask_operationFails() {
  Repo repo;
  Task task;

  assert(!repo.update(task));
}

void test_Repo_remove_existingTask_operationSucceeds() {
  Repo repo;
  Task task;

  repo.add(task);
  assert(repo.remove(task.title()));
}

void test_Repo_remove_inexistentTask_operationFails() {
  Repo repo;
  Task task;

  assert(!repo.remove(task.title()));
}

void test_FileRepo_setFilePath_anyFilePath_repoFilePathIsUpdated() {
  FileRepo repo;
  repo.setFilePath("abc");

  assert(repo.filePath() == "abc");
}

void test_FileRepo_add_inexistentTask_operationSucceeds() {
  FileRepo repo{"a.txt"};
  Task task;

  assert(repo.add(task));
  std::remove("a.txt");
}

void test_FileRepo_add_existingTask_operationFails() {
  FileRepo repo{"a.txt"};
  Task task;

  repo.add(task);
  assert(!repo.add(task));
  std::remove("a.txt");
}

void test_FileRepo_update_existingTask_operationSucceeds() {
  FileRepo repo{"a.txt"};
  Task task;

  repo.add(task);
  assert(repo.update(task));
  std::remove("a.txt");
}

void test_FileRepo_update_inexistentTask_operationFails() {
  FileRepo repo{"a.txt"};
  Task task;

  assert(!repo.update(task));
  std::remove("a.txt");
}

void test_FileRepo_remove_existingTask_operationSucceeds() {
  FileRepo repo{"a.txt"};
  Task task;

  repo.add(task);
  assert(repo.remove(task.title()));
  std::remove("a.txt");
}

void test_FileRepo_remove_inexistentTask_operationFails() {
  FileRepo repo{"a.txt"};
  Task task;

  assert(!repo.remove(task.title()));
  std::remove("a.txt");
}

void test_FileRepo_data_emptyRepo_DataIsEmpty() {
  FileRepo repo{"a.txt"};

  assert(repo.data().empty());
}

void test_Services_setMode_anyInput_servicesModeIsUpdated() {
  Services services;

  services.setMode("A");
  assert(services.mode() == "A");
}

void test_Services_setFilePath_anyInput_servicesFilePathIsUpdated() {
  Services services;

  services.setFilePath("abc");
  assert(services.filePath() == "abc");
}

void test_Services_add_inexistentTask_operationSucceeds() {
  Services services{"A", "a.txt"};
  Task task;

  assert(services.add(task));
  std::remove("a.txt");
}

void test_Services_add_existingTask_operationFails() {
  Services services{"A", "a.txt"};
  Task task;

  services.add(task);
  assert(!services.add(task));
  std::remove("a.txt");
}

void test_Services_update_existingTask_operationSucceeds() {
  Services services{"A", "a.txt"};
  Task task;

  services.add(task);
  assert(services.update(task));
  std::remove("a.txt");
}

void test_Services_update_inexistentTask_operationFails() {
  Services services{"A", "a.txt"};
  Task task;

  assert(!services.update(task));
  std::remove("a.txt");
}

void test_Services_remove_existingTask_operationSucceeds() {
  Services services{"A", "a.txt"};
  Task task;

  services.add(task);
  assert(services.remove(task.title()));
  std::remove("a.txt");
}

void test_Services_remove_inexistentTask_operationFails() {
  Services services{"A", "a.txt"};
  Task task;

  assert(!services.remove(task.title()));
  std::remove("a.txt");
}

void test_Services_allTasks_wrongMode_throwsWrongModeException() {
  Services services{"B", "a.txt"};

  try {
    services.allTasks();
  } catch (WrongModeException&) {
    assert(true);
  }

  std::remove("a.txt");
}

void test_Services_allTasks_emptyServicesAndCorrectMode_allTasksVectorIsEmpty() {
  Services services{"A", "a.txt"};

  std::vector<Task> expectedOutput;
  auto output = services.allTasks();

  assert(output == expectedOutput);
  std::remove("a.txt");
}

void test_Services_servantTasks_wrongMode_throwsWrongModeException() {
  Services services{"A", "a.txt"};

  try {
    services.servantTasks();
  } catch (WrongModeException&) {
    assert(true);
  }

  std::remove("a.txt");
}

void test_Services_servantTasks_emptyServicesAndCorrectMode_allTasksVectorIsEmpty() {
  Services services{"B", "a.txt"};

  std::vector<Task> expectedOutput;
  auto output = services.servantTasks();

  assert(output == expectedOutput);
  std::remove("a.txt");
}

void test_Services_next_emptyServices_returnsFalse() {
  Services services{"B", "a.txt"};
  
  assert(!services.next().first);
  std::remove("a.txt");
}

void test_Services_next_nonEmptyServices_returnsFirstTask() {
  Services services{"A", "a.txt"};
  Task task;

  services.add(task);
  services.setMode("B");

  assert(services.next().second == task);
  std::remove("a.txt");
}

void test_Services_next_lastElementinServices_wrapsAround() {
  Services services{"A", "a.txt"};
  Task task;

  services.add(task);
  services.setMode("B");

  services.next();
  assert(services.next().second == task);
  std::remove("a.txt");
}

void test_Services_save_inexsitentTask_operationFails() {
  Services services{"B", "a.txt"};
  Task task;

  assert(!services.save(task.title()));
  std::remove("a.txt");
}

void test_Services_save_existingTask_operationSucceeds() {
  Services services{"A", "a.txt"};
  Task task;

  services.add(task);
  services.setMode("B");

  assert(services.save(task.title()));
  std::remove("a.txt");
}

void test_Services_tasksByTimesPerformed_existingItems_operationFiltersElementsProperly() {
  Services services{"A", "a.txt"};

  Task taskA{"1", "a", "1", 1, "a"};
  Task taskB{"2", "b", "1", 1, "b"};

  services.add(taskA);
  services.add(taskB);

  services.setMode("B");

  auto tasks = services.tasksByTimesPerformed("b", 2);

  assert(tasks.size() == 1 && tasks.at(0) == taskB);
  std::remove("a.txt");
}

void runAllTests() {
  test_Task_title_noInput_returnsTaskTitle();
  test_Task_type_noInput_returnsTaskType();
  test_Task_lastPerformed_noInput_returnsTaskLastPerformed();
  test_Task_timesPerformed_noInput_returnsTaskTimesPerformed();
  test_Task_vision_noInput_returnsTaskVision();

  test_Task_equalsOperator_anyTask_taskIsEqualToItself();

  test_Domain_trimString_paddedString_stringIsTrimmed();
  test_Domain_splitString_stringWithMultipleTokens_allTokensAreSeparated();

  test_Repo_ctorWithVector_anyVector_repoDataEqualsOriginalVector();
  test_Repo_add_inexistentTask_operationSucceeds();
  test_Repo_add_existingTask_operationFails();
  test_Repo_update_existingTask_operationSucceeds();
  test_Repo_update_inexistentTask_operationFails();
  test_Repo_remove_existingTask_operationSucceeds();
  test_Repo_remove_inexistentTask_operationFails();

  test_FileRepo_setFilePath_anyFilePath_repoFilePathIsUpdated();
  test_FileRepo_add_inexistentTask_operationSucceeds();
  test_FileRepo_add_existingTask_operationFails();
  test_FileRepo_update_existingTask_operationSucceeds();
  test_FileRepo_update_inexistentTask_operationFails();
  test_FileRepo_remove_existingTask_operationSucceeds();
  test_FileRepo_remove_inexistentTask_operationFails();
  test_FileRepo_data_emptyRepo_DataIsEmpty();

  test_Services_setMode_anyInput_servicesModeIsUpdated();
  test_Services_setFilePath_anyInput_servicesFilePathIsUpdated();
  test_Services_add_inexistentTask_operationSucceeds();
  test_Services_add_existingTask_operationFails();
  test_Services_update_existingTask_operationSucceeds();
  test_Services_update_inexistentTask_operationFails();
  test_Services_remove_existingTask_operationSucceeds();
  test_Services_remove_inexistentTask_operationFails();

  test_Services_allTasks_wrongMode_throwsWrongModeException();
  test_Services_allTasks_emptyServicesAndCorrectMode_allTasksVectorIsEmpty();
  test_Services_servantTasks_wrongMode_throwsWrongModeException();
  test_Services_servantTasks_emptyServicesAndCorrectMode_allTasksVectorIsEmpty();

  test_Services_next_emptyServices_returnsFalse();
  test_Services_next_nonEmptyServices_returnsFirstTask();
  test_Services_next_lastElementinServices_wrapsAround();

  test_Services_save_inexsitentTask_operationFails();
  test_Services_save_existingTask_operationSucceeds();
  
  test_Services_tasksByTimesPerformed_existingItems_operationFiltersElementsProperly();
}

int main() {
  runAllTests();
  std::cout << "All tests passed.\n";
}