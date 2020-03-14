#ifndef ACTION_H
#define ACTION_H

#include "domain.h"

typedef enum {
  Add,
  Delete,
  Update
} ActionType;

typedef struct {
  ActionType type;
  Ingredient ingredient;
  Ingredient old_ingredient;
} Action;

// Creates a new action representing the addition of the specified ingredient.
Action new_add(const Ingredient* ingredient);

// Creates a new action representing the deletion of the specified ingredient.
Action new_delete(const Ingredient* ingredient);

// Creates a new action representing the updating `old_ingredient` to `ingredient`.
Action new_update(const Ingredient* ingredient, const Ingredient* old_ingredient);

// Returns the action that would undo the specified action.
Action inverse_action(const Action* action);

typedef struct {
  int length, capacity;
  Action* data;
} ActionList;

// Creates a new ActionList with 0 actions.
ActionList new_action_list();

// Frees any memory held by the specified ActionList.
void free_action_list(ActionList* action_list);

// Adds the specified action to the specified action list.
void action_list_push(ActionList* action_list, const Action* action);

// Attempts to remove the most recently added action from the specified action list.
// If the action list is empty, the function returns `false`.
// If the action list is not empty, the function returns `true`, and
// the removed action is copied into `action_buf`.
bool action_list_pop(ActionList* action_list, Action* action_buf);

// Clears the specified action, removing all elements.
// Post: *action_list == new_action_list()
void action_list_clear(ActionList* action_list);

#endif