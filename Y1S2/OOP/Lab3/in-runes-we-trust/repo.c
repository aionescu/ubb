#include <stdbool.h>
#include "domain.h"
#include "action.h"
#include "repo.h"

Repo new_repo() {
  Repo repo;
  repo.shelf = new_shelf();
  repo.done = new_action_list();
  repo.undone = new_action_list();

  return repo;
}

void free_repo(Repo* repo) {
  free_shelf(&repo->shelf);
  free_action_list(&repo->done);
  free_action_list(&repo->undone);
}

static bool silent = false;

bool repo_add_ingredient(Repo* repo, const Ingredient* new_ingredient) {
  SHELF_FOR(ingredient, &repo->shelf)
    if (ingredient->id == new_ingredient->id)
      return false;

  if (!silent) {
    Action action = new_add(new_ingredient);
    action_list_push(&repo->done, &action);
    action_list_clear(&repo->undone);
  }

  shelf_add_to_end(&repo->shelf, new_ingredient);
  return true;
}

bool repo_remove_ingredient(Repo* repo, int ingredient_id) {
  SHELF_FOR(ingredient, &repo->shelf)
    if (ingredient->id == ingredient_id) {
      if (!silent) {
        Action action = new_delete(ingredient);
        action_list_push(&repo->done, &action);
        action_list_clear(&repo->undone);
      }

      shelf_remove_at(&repo->shelf, ingredient - repo->shelf.data);
      return true;
    }

  return false;
}

bool repo_update_ingredient(Repo* repo, const Ingredient* new_ingredient) {
  SHELF_FOR_MUT(ingredient, &repo->shelf)
    if (ingredient->id == new_ingredient->id) {
      if (!silent) {
        Action action = new_update(new_ingredient, ingredient);
        action_list_push(&repo->done, &action);
        action_list_clear(&repo->undone);
      }

      *ingredient = *new_ingredient;
      return true;
    }

  return false;
}

bool repo_undo(Repo* repo) {
  Action action;

  if (!action_list_pop(&repo->done, &action))
    return false;

  Action inverse = inverse_action(&action);
  action_list_push(&repo->undone, &inverse);

  silent = true;

  switch (action.type) {
    case Add:
      repo_remove_ingredient(repo, action.ingredient.id);
      break;
    case Delete:
      repo_add_ingredient(repo, &action.ingredient);
      break;
    default: // case Update:
      repo_update_ingredient(repo, &action.old_ingredient);
      break;
  }

  silent = false;
  return true;
}

bool repo_redo(Repo* repo) {
  Action action;

  if (!action_list_pop(&repo->undone, &action))
    return false;

  Action inverse = inverse_action(&action);
  action_list_push(&repo->done, &inverse);

  silent = true;

  switch (action.type) {
    case Add:
      repo_remove_ingredient(repo, action.ingredient.id);
      break;
    case Delete:
      repo_add_ingredient(repo, &action.ingredient);
      break;
    default: // case Update:
      repo_update_ingredient(repo, &action.old_ingredient);
      break;
  }

  silent = false;
  return true;
}

const Shelf* repo_data(const Repo* repo) {
  return &repo->shelf;
}