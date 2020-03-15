#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "Domain.h"
#include "Repo.h"
#include "Controller.h"

Controller newController() {
  Controller controller;
  controller.repo = newRepo();
  controller.done = newActionList();
  controller.undone = newActionList();

  return controller;
}

void freeController(Controller* controller) {
  freeRepo(&controller->repo);
  freeActionList(&controller->done);
  freeActionList(&controller->undone);
}

bool controllerAddIngredient(Controller* controller, const Ingredient* ingredient) {
  bool success = repoAddIngredient(&controller->repo, ingredient);

  if (success) {
    Action action = newAdd(ingredient);
    actionListPush(&controller->done, &action);
    actionListClear(&controller->undone);
  }

  return success;
}

bool controllerUpdateIngredient(Controller* controller, const Ingredient* ingredient) {
  Ingredient oldIngredient;

  bool success = repoUpdateIngredient(&controller->repo, ingredient, &oldIngredient);

  if (success) {
    Action action = newUpdate(ingredient, &oldIngredient);
    actionListPush(&controller->done, &action);
    actionListClear(&controller->undone);
  }

  return success;
}

bool controllerRemoveIngredient(Controller* controller, int ingredientId) {
  int index;
  Ingredient ingredient;

  bool success = repoRemoveIngredient(&controller->repo, ingredientId, &index, &ingredient);

  if (success) {
    Action action = newDelete(&ingredient);
    actionListPush(&controller->done, &action);
    actionListClear(&controller->undone);
  }

  return success;
}

bool controllerUndo(Controller* controller) {
  Action action;
  Repo* repo = &controller->repo;

  if (!actionListPop(&controller->done, &action))
    return false;

  Action inverse = inverseAction(&action);
  actionListPush(&controller->undone, &inverse);

  switch (action.type) {
    case Add:
      repoRemoveIngredient(repo, action.ingredient.id, NULL, NULL);
      break;
    case Delete:
      repoAddIngredient(repo, &action.ingredient);
      break;
    default: // case Update:
      repoUpdateIngredient(repo, &action.oldIngredient, NULL);
      break;
  }

  return true;
}

bool controllerRedo(Controller* controller) {
  Action action;
  Repo* repo = &controller->repo;

  if (!actionListPop(&controller->undone, &action))
    return false;

  Action inverse = inverseAction(&action);
  actionListPush(&controller->done, &inverse);

  switch (action.type) {
    case Add:
      repoRemoveIngredient(repo, action.ingredient.id, NULL, NULL);
      break;
    case Delete:
      repoAddIngredient(repo, &action.ingredient);
      break;
    default: // case Update:
      repoUpdateIngredient(repo, &action.oldIngredient, NULL);
      break;
  }

  return true;
}

const Shelf* controllerData(const Controller* controller) {
  return repoData(&controller->repo);
}