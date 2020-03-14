#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "domain.h"
#include "repo.h"
#include "controller.h"

Controller new_controller() {
  Controller controller;
  controller.repo = new_repo();

  return controller;
}

void free_controller(Controller* controller) {
  free_repo(&controller->repo);      
}

bool controller_add_ingredient(Controller* controller, const Ingredient* ingredient) {
  return repo_add_ingredient(&controller->repo, ingredient);
}

bool controller_update_ingredient(Controller* controller, const Ingredient* ingredient) {
  return repo_update_ingredient(&controller->repo, ingredient);
}

bool controller_remove_ingredient(Controller* controller, int ingredient_id) {
  return repo_remove_ingredient(&controller->repo, ingredient_id);
}

bool controller_undo(Controller* controller) {
  return repo_undo(&controller->repo);
}

bool controller_redo(Controller* controller) {
  return repo_redo(&controller->repo);
}

const Shelf* controller_data(const Controller* controller) {
  return repo_data(&controller->repo);
}