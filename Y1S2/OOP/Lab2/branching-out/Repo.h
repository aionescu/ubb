#ifndef REPO_H
#define REPO_H

#include <stdbool.h>
#include "Domain.h"

#define SHELF_SIZE 100

typedef struct {
  Ingredient ingredients[SHELF_SIZE];
  int ingredientCount;
} Repo;

bool repoAddIngredient(Repo* repo, const Ingredient* ingredient);
bool repoRemoveIngredient(Repo* repo, int ingredientId);
bool repoUpdateIngredient(Repo* repo, const Ingredient* newIngredient);

void repoForEach(const Repo* repo, void (*action)(const Ingredient* ingredient));

#endif