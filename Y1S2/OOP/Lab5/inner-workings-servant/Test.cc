#include <cassert>
#include <iostream>
#include <sstream>
#include "Domain.hh"
#include "Vector.hh"
#include "Repo.hh"
#include "Services.hh"
#include "UI.hh"

void test_vector_append_anyInput_vectorGrows() {
  Vector<int> vector;

  vector.append(0);
  assert(vector.length() == 1);
}

void test_vector_remove_existingElement_vectorShrinks() {
  Vector<int> vector;
  vector.append(0);

  vector.remove(0);
  assert(vector.length() == 0);
}

void test_vector_operatorSubscript_invalidIndex_throwsException() {
  Vector<int> vector;

  try {
    vector[0];
    assert(false);
  } catch (std::out_of_range&) {}
}

void test_repo_add_newTask_operationSucceeds() {
  Repo repo;

  assert(repo.add(Task{}));
}

void test_repo_add_existingTask_operationFails() {
  Repo repo;
  Task task;

  repo.add(task);
  assert(!repo.add(task));
}

void test_repo_remove_existingTask_operationSucceeds() {
  Repo repo;
  Task task;

  repo.add(task);
  assert(repo.remove(task.title()));
}

void test_repo_remove_inexistentTask_operationFails() {
  Repo repo;

  assert(!repo.remove(Task{}.title()));
}

void test_services_mode_anyInput_changesMode() {
  Services services;
  services.setMode("A");

  assert(services.mode() == "A");
}

void test_services_add_wrongMode_throwsException() {
  Services services;

  try {
    services.add(Task{});
    assert(false);
  } catch (WrongModeException&) {}
}

void test_services_add_newTask_operationSucceeds() {
  Services services;
  services.setMode("A");

  assert(services.add(Task{}));
}

void test_services_add_existingTask_operationFails() {
  Services services;
  services.setMode("A");

  Task task;
  services.add(task);

  assert(!services.add(task));
}

void test_services_remove_existingTask_operationSucceeds() {
  Services services;
  services.setMode("A");

  Task task;
  services.add(task);

  assert(services.remove(task.title()));
}

void test_services_remove_inexistentTask_operationFails() {
  Services services;
  services.setMode("A");

  assert(!services.remove(Task{}.title()));
}

void runIntegrationTest(std::string input, std::string expectedOutput) {
  std::istringstream inStream{input};
  std::ostringstream outStream;

  UI ui{inStream, outStream, false};
  ui.mainLoop();
  
  assert(outStream.str() == expectedOutput);
}

void integrationTest1() {
  std::string input = "\
mode A\n\
add 100, abc, 01-01-2010, 123, zzz\n\
add 200, def, 01-01-2010, 123, zzz\n\
add 300, abc, 01-01-2000, 456, zzz\n\
add 400, def, 01-01-2000, 456, zzz\n\
mode B\n\
list abc, 300\n\
exit\n";

  std::string expectedOutput = "100, abc, 01-01-2010, 123, zzz\n";

  runIntegrationTest(input, expectedOutput);
}

void integrationTest2() {
  std::string input = "inexistingCommand\nexit\n";
  std::string expectedOutput = "Command not recognized.\n";

  runIntegrationTest(input, expectedOutput);
}

void integrationTest3() {
  std::string input = "\
mode A\n\
add 123, abc, 01-01-2000, 456, def\n\
mode B\n\
save 123\n\
mylist\n\
exit\n";

  std::string expectedOutput = "123, abc, 01-01-2000, 456, def\n";

  runIntegrationTest(input, expectedOutput);
}

void integrationTest4() {
  std::string input = "\
mode A\n\
add 123, abc, 01-01-2000, 456, def\n\
mode B\n\
next\n\
exit\n";

  std::string expectedOutput = "123, abc, 01-01-2000, 456, def\n";

  runIntegrationTest(input, expectedOutput);
}

void runAllTests() {
  test_vector_append_anyInput_vectorGrows();
  test_vector_remove_existingElement_vectorShrinks();
  test_vector_operatorSubscript_invalidIndex_throwsException();

  test_repo_add_newTask_operationSucceeds();
  test_repo_add_existingTask_operationFails();
  test_repo_remove_existingTask_operationSucceeds();
  test_repo_remove_inexistentTask_operationFails();

  test_services_mode_anyInput_changesMode();
  test_services_add_wrongMode_throwsException();
  test_services_add_newTask_operationSucceeds();
  test_services_add_existingTask_operationFails();
  test_services_remove_existingTask_operationSucceeds();
  test_services_remove_inexistentTask_operationFails();

  integrationTest1();
  integrationTest2();
  integrationTest3();
  integrationTest4();
}

int main() {
  runAllTests();
  std::cout << "All tests passed.\n";
}