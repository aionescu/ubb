#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "Domain.h"
#include "Repo.h"
#include "Controller.h"

bool controllerAddIngredient(Controller* controller, const Ingredient* ingredient) {
  return repoAddIngredient(&controller->repo, ingredient);
}

bool controllerUpdateIngredient(Controller* controller, const Ingredient* ingredient) {
  return repoUpdateIngredient(&controller->repo, ingredient);
}

bool controllerRemoveIngredient(Controller* controller, int ingredientId) {
  return repoRemoveIngredient(&controller->repo, ingredientId);
}

IngredientArray controllerData(const Controller* controller) {
  return repoData(&controller->repo);
}