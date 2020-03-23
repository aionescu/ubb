#include <iostream>

#include <cassert>
#include "Domain.hh"
#include "Vector.hh"
#include "Repo.hh"
#include "Services.hh"

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
}

int main() {
  runAllTests();
  std::cout << "All tests passed.\n";
}