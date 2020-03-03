#ifndef __REPO_H__
#define __REPO_H__

#include <stdbool.h>
#include "Domain.h"

#define SHELF_SIZE 100

typedef struct {
  Ingredient ingredients[SHELF_SIZE];
  int ingredientCount;
} Repo;

bool addIngredient(Repo* repo, Ingredient ingredient);
bool removeIngredient(Repo* repo, int ingredientId);
bool updateIngredient(Repo* repo, Ingredient newIngredient);

#endif
