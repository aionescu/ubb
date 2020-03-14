#include <stdbool.h>
#include "Domain.h"
#include "Action.h"
#include "Repo.h"

Repo newRepo() {
  Repo repo;
  repo.shelf = newShelf();
  repo.done = newActionList();
  repo.undone = newActionList();

  return repo;
}

void freeRepo(Repo* repo) {
  freeShelf(&repo->shelf);
  freeActionList(&repo->done);
  freeActionList(&repo->undone);
}

static bool silent = false;

bool repoAddIngredient(Repo* repo, const Ingredient* newIngredient) {
  SHELF_FOR(ingredient, &repo->shelf)
    if (ingredient->id == newIngredient->id)
      return false;

  if (!silent) {
    Action action = newAdd(newIngredient);
    actionListPush(&repo->done, &action);
    actionListClear(&repo->undone);
  }

  shelfAddToEnd(&repo->shelf, newIngredient);
  return true;
}

bool repoRemoveIngredient(Repo* repo, int ingredientId) {
  SHELF_FOR(ingredient, &repo->shelf)
    if (ingredient->id == ingredientId) {
      if (!silent) {
        Action action = newDelete(ingredient);
        actionListPush(&repo->done, &action);
        actionListClear(&repo->undone);
      }

      shelfRemoveAt(&repo->shelf, ingredient - repo->shelf.data);
      return true;
    }

  return false;
}

bool repoUpdateIngredient(Repo* repo, const Ingredient* newIngredient) {
  SHELF_FOR_MUT(ingredient, &repo->shelf)
    if (ingredient->id == newIngredient->id) {
      if (!silent) {
        Action action = newUpdate(newIngredient, ingredient);
        actionListPush(&repo->done, &action);
        actionListClear(&repo->undone);
      }

      *ingredient = *newIngredient;
      return true;
    }

  return false;
}

bool repoUndo(Repo* repo) {
  Action action;

  if (!actionListPop(&repo->done, &action))
    return false;

  Action inverse = inverseAction(&action);
  actionListPush(&repo->undone, &inverse);

  silent = true;

  switch (action.type) {
    case Add:
      repoRemoveIngredient(repo, action.ingredient.id);
      break;
    case Delete:
      repoAddIngredient(repo, &action.ingredient);
      break;
    default: // case Update:
      repoUpdateIngredient(repo, &action.oldIngredient);
      break;
  }

  silent = false;
  return true;
}

bool repoRedo(Repo* repo) {
  Action action;

  if (!actionListPop(&repo->undone, &action))
    return false;

  Action inverse = inverseAction(&action);
  actionListPush(&repo->done, &inverse);

  silent = true;

  switch (action.type) {
    case Add:
      repoRemoveIngredient(repo, action.ingredient.id);
      break;
    case Delete:
      repoAddIngredient(repo, &action.ingredient);
      break;
    default: // case Update:
      repoUpdateIngredient(repo, &action.oldIngredient);
      break;
  }

  silent = false;
  return true;
}

const Shelf* repoData(const Repo* repo) {
  return &repo->shelf;
}