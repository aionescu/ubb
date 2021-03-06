#include <stdbool.h>
#include "Domain.h"
#include "Action.h"
#include "Repo.h"

Repo newRepo() {
  Repo repo;
  repo.shelf = newShelf();

  return repo;
}

void freeRepo(Repo* repo) {
  freeShelf(&repo->shelf);
}

bool repoAddIngredient(Repo* repo, const Ingredient* newIngredient) {
  SHELF_FOR(ingredient, &repo->shelf)
    if (ingredient->id == newIngredient->id)
      return false;

  shelfAddToEnd(&repo->shelf, newIngredient);
  return true;
}

bool repoRemoveIngredient(Repo* repo, int ingredientId, int* indexBuffer, Ingredient* ingredientBuffer) {
  SHELF_FOR(ingredient, &repo->shelf)
    if (ingredient->id == ingredientId) {
      int index = ingredient - repo->shelf.data;

      if (indexBuffer)
        *indexBuffer = index;

      if (ingredientBuffer)
        *ingredientBuffer = *ingredient;

      shelfRemoveAt(&repo->shelf, index);
      return true;
    }

  return false;
}

bool repoUpdateIngredient(Repo* repo, const Ingredient* newIngredient, Ingredient* ingredientBuffer) {
  SHELF_FOR_MUT(ingredient, &repo->shelf)
    if (ingredient->id == newIngredient->id) {

      if (ingredientBuffer)
        *ingredientBuffer = *ingredient;

      *ingredient = *newIngredient;
      return true;
    }

  return false;
}

const Shelf* repoData(const Repo* repo) {
  return &repo->shelf;
}