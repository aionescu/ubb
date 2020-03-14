#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "Domain.h"
#include "Repo.h"
#include "Controller.h"

Controller newController() {
  Controller controller;
  controller.repo = newRepo();

  return controller;
}

void freeController(Controller* controller) {
  freeRepo(&controller->repo);      
}

bool controllerAddIngredient(Controller* controller, const Ingredient* ingredient) {
  return repoAddIngredient(&controller->repo, ingredient);
}

bool controllerUpdateIngredient(Controller* controller, const Ingredient* ingredient) {
  return repoUpdateIngredient(&controller->repo, ingredient);
}

bool controllerRemoveIngredient(Controller* controller, int ingredientId) {
  return repoRemoveIngredient(&controller->repo, ingredientId);
}

bool controllerUndo(Controller* controller) {
  return repoUndo(&controller->repo);
}

bool controllerRedo(Controller* controller) {
  return repoRedo(&controller->repo);
}

const Shelf* controllerData(const Controller* controller) {
  return repoData(&controller->repo);
}