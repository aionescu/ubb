#ifndef ACTION_H
#define ACTION_H

#include "Domain.h"

typedef enum {
  Add,
  Delete,
  Update
} ActionType;

typedef struct {
  ActionType type;
  Ingredient ingredient;
  Ingredient oldIngredient;
} Action;

// Creates a new action representing the addition of the specified ingredient.
Action newAdd(const Ingredient* ingredient);

// Creates a new action representing the deletion of the specified ingredient.
Action newDelete(const Ingredient* ingredient);

// Creates a new action representing the updating `oldIngredient` to `ingredient`.
Action newUpdate(const Ingredient* ingredient, const Ingredient* oldIngredient);

// Returns the action that would undo the specified action.
Action inverseAction(const Action* action);

typedef struct {
  int length, capacity;
  Action* data;
} ActionList;

// Creates a new ActionList with 0 actions.
ActionList newActionList();

// Frees any memory held by the specified ActionList.
void freeActionList(ActionList* actionList);

// Adds the specified action to the specified action list.
void actionListPush(ActionList* actionList, const Action* action);

// Attempts to remove the most recently added action from the specified action list.
// If the action list is empty, the function returns `false`.
// If the action list is not empty, the function returns `true`, and
// the removed action is copied into `actionBuf`.
bool actionListPop(ActionList* actionList, Action* actionBuf);

// Clears the specified action, removing all elements.
// Post: *actionList == newActionList()
void actionListClear(ActionList* actionList);

#endif