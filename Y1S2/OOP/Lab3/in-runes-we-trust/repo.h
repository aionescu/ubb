#ifndef REPO_H
#define REPO_H

#include <stdbool.h>
#include "domain.h"
#include "action.h"

typedef struct {
  Shelf shelf;
  ActionList done, undone;
} Repo;

// Initializes a new repository with 0 ingredients and no actions performed.
Repo new_repo();

// Frees any memory held by the repository.
void free_repo(Repo* repo);

// Adds an ingredient to the repository, unless an ingredient with the same ID already exists.
// Returns: `false` if an ingredient with the same ID already exists, otherwise `true`.
bool repo_add_ingredient(Repo* repo, const Ingredient* new_ingredient);

// Attempts to remove the ingredient with the specified ID from the repository.
// Returns: `false` if no ingredient exists with the specified ID,
// otherwise `true`.
bool repo_remove_ingredient(Repo* repo, int ingredient_id);

// Attempts to update the ingredient with the specified ID.
// Returns: `false` if no ingredient exists with the specified ID,
// otherwise `true`.
bool repo_update_ingredient(Repo* repo, const Ingredient* new_ingredient);

// Attempts to undo the last performed action.
// Returns: `false` if no actions were performed, otherwise `true`.
bool repo_undo(Repo* repo);

// Attempts to redo the last undone action.
// Returns: `false` if no actions were undone, otherwise `true`.
bool repo_redo(Repo* repo);

// Returns a read-only view of the repository's data.
const Shelf* repo_data(const Repo* repo);

#endif