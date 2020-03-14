#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include "Action.h"

Action newAdd(const Ingredient* ingredient) {
  Action action;
  action.type = Add;
  action.ingredient = *ingredient;
  action.oldIngredient = zeroedIngredient();

  return action;
}

Action newDelete(const Ingredient* ingredient) {
  Action action;
  action.type = Delete;
  action.ingredient = *ingredient;
  action.oldIngredient = zeroedIngredient();

  return action;
}

Action newUpdate(const Ingredient* ingredient, const Ingredient* oldIngredient) {
  Action action;
  action.type = Update;
  action.ingredient = *ingredient;
  action.oldIngredient = *oldIngredient;

  return action;
}

Action inverseAction(const Action* action) {
  switch (action->type) {
    case Add:
      return newDelete(&action->ingredient);
    case Delete:
      return newAdd(&action->ingredient);
    default: // case Update:
      return newUpdate(&action->oldIngredient, &action->ingredient);
  }
}

ActionList newActionList() {
  ActionList actionList;
  actionList.length = actionList.capacity = 0;
  actionList.data = NULL;

  return actionList;
}

void freeActionList(ActionList* actionList) {
  actionList->length = actionList->capacity = 0;

  if (actionList->data) {
    free(actionList->data);
    actionList->data = NULL;
  }
}

void actionListGrow(ActionList* actionList) {
  if (actionList->capacity == 0) {
    if (actionList->data)
      free(actionList->data);
      
    actionList->data = malloc(sizeof(Action));
    actionList->capacity = 1;
  } else {
    int newCapacity = actionList->capacity * 2;
    Action* newBuf = malloc(newCapacity * sizeof(Action));
    memcpy(newBuf, actionList->data, actionList->length * sizeof(Action));

    free(actionList->data);
    actionList->data = newBuf;
    actionList->capacity = newCapacity;
  }
}

bool actionListNeedsToGrow(const ActionList* actionList) {
  return actionList->capacity == 0 || actionList->length == actionList->capacity;
}

void actionListPush(ActionList* actionList, const Action* action) {
  if (actionListNeedsToGrow(actionList))
    actionListGrow(actionList);

  actionList->data[actionList->length++] = *action;
}

bool actionListPop(ActionList* actionList, Action* action) {
  if (actionList->length == 0)
    return false;
  
  *action = actionList->data[--actionList->length];
  return true;
}

void actionListClear(ActionList* actionList) {
  freeActionList(actionList);
  *actionList = newActionList();
}