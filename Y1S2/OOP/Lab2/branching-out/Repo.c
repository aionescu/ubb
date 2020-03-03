#include <stdbool.h>
#include "Repo.h"

bool addIngredient(Repo* repo, Ingredient ingredient) {
  if (repo->ingredientCount == SHELF_SIZE)
    return false;

  for (int i = 0; i < repo->ingredientCount; ++i)
    if (repo->ingredients[i].id == ingredient.id)
      return false;

  repo->ingredients[repo->ingredientCount] = ingredient;
  repo->ingredientCount++;
  
  return true;
}

bool removeIngredient(Repo* repo, int ingredientId) {
  for (int i = 0; i < repo->ingredientCount; ++i)
    if (repo->ingredients[i].id == ingredientId) {
      for (int j = i; j < repo->ingredientCount - 1; ++j)
        repo->ingredients[j] = repo->ingredients[j + 1];

      repo->ingredientCount--;
      return true;
    }

  return false;
}

bool updateIngredient(Repo* repo, Ingredient newIngredient) {
  for (int i = 0; i < repo->ingredientCount; ++i)
    if (repo->ingredients[i].id == newIngredient.id) {
      repo->ingredients[i] = newIngredient;
      return true;
    }

  return false;
}
