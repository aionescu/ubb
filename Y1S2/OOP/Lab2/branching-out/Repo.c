#include <stdbool.h>
#include "Repo.h"

bool repoAddIngredient(Repo* repo, const Ingredient* ingredient) {
  if (repo->ingredientCount == SHELF_SIZE)
    return false;

  for (int i = 0; i < repo->ingredientCount; ++i)
    if (repo->ingredients[i].id == ingredient->id)
      return false;

  repo->ingredients[repo->ingredientCount] = *ingredient;
  repo->ingredientCount++;
  
  return true;
}

bool repoRemoveIngredient(Repo* repo, int ingredientId) {
  for (int i = 0; i < repo->ingredientCount; ++i)
    if (repo->ingredients[i].id == ingredientId) {
      for (int j = i; j < repo->ingredientCount - 1; ++j)
        repo->ingredients[j] = repo->ingredients[j + 1];

      repo->ingredientCount--;
      return true;
    }

  return false;
}

bool repoUpdateIngredient(Repo* repo, const Ingredient* newIngredient) {
  for (int i = 0; i < repo->ingredientCount; ++i)
    if (repo->ingredients[i].id == newIngredient->id) {
      repo->ingredients[i] = *newIngredient;
      return true;
    }

  return false;
}

IngredientArray repoData(const Repo* repo) {
  IngredientArray array;
  array.length = repo->ingredientCount;
  array.data = repo->ingredients;

  return array;
}