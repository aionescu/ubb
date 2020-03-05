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

void controllerListAll(const Controller* controller) {
  repoForEach(&controller->repo, printIngredient);
}

static const char* currentIntendedUse;

void printByIntendedUse(const Ingredient* ingredient) {
  if (!strcmp(ingredient->intendedUse, currentIntendedUse))
    printIngredient(ingredient);
}

void controllerListByIntendedUse(const Controller* controller, const char* intendedUse) {
  currentIntendedUse = intendedUse;
  repoForEach(&controller->repo, printByIntendedUse);

  currentIntendedUse = NULL;
}