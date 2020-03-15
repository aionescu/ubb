#ifndef REPO_H
#define REPO_H

#include <stdbool.h>
#include "Domain.h"
#include "Action.h"

typedef struct {
  Shelf shelf;
} Repo;

// Initializes a new repository with 0 ingredients and no actions performed.
Repo newRepo();

// Frees any memory held by the repository.
void freeRepo(Repo* repo);

// Adds an ingredient to the repository, unless an ingredient with the same ID already exists.
// Returns: `false` if an ingredient with the same ID already exists, otherwise `true`.
bool repoAddIngredient(Repo* repo, const Ingredient* newIngredient);

// Attempts to remove the ingredient with the specified ID from the repository.
// Returns: `false` if no ingredient exists with the specified ID,
// otherwise `true`.
bool repoRemoveIngredient(Repo* repo, int ingredientId, int* indexBuf, Ingredient* ingredientBuf);

// Attempts to update the ingredient with the specified ID.
// Returns: `false` if no ingredient exists with the specified ID,
// otherwise `true`.
bool repoUpdateIngredient(Repo* repo, const Ingredient* newIngredient, Ingredient* ingredientBuf);

// Returns a read-only view of the repository's data.
const Shelf* repoData(const Repo* repo);

#endif