#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include "action.h"

Action new_add(const Ingredient* ingredient) {
  Action action;
  action.type = Add;
  action.ingredient = *ingredient;
  action.old_ingredient = zeroed_ingredient();

  return action;
}

Action new_delete(const Ingredient* ingredient) {
  Action action;
  action.type = Delete;
  action.ingredient = *ingredient;
  action.old_ingredient = zeroed_ingredient();

  return action;
}

Action new_update(const Ingredient* ingredient, const Ingredient* old_ingredient) {
  Action action;
  action.type = Update;
  action.ingredient = *ingredient;
  action.old_ingredient = *old_ingredient;

  return action;
}

Action inverse_action(const Action* action) {
  switch (action->type) {
    case Add:
      return new_delete(&action->ingredient);
    case Delete:
      return new_add(&action->ingredient);
    default: // case Update:
      return new_update(&action->old_ingredient, &action->ingredient);
  }
}

ActionList new_action_list() {
  ActionList action_list;
  action_list.length = action_list.capacity = 0;
  action_list.data = NULL;

  return action_list;
}

void free_action_list(ActionList* action_list) {
  action_list->length = action_list->capacity = 0;

  if (action_list->data) {
    free(action_list->data);
    action_list->data = NULL;
  }
}

void action_list_grow(ActionList* action_list) {
  if (action_list->capacity == 0) {
    if (action_list->data)
      free(action_list->data);
      
    action_list->data = malloc(sizeof(Action));
    action_list->capacity = 1;
  } else {
    int new_capacity = action_list->capacity * 2;
    Action* new_buf = malloc(new_capacity * sizeof(Action));
    memcpy(new_buf, action_list->data, action_list->length * sizeof(Action));

    free(action_list->data);
    action_list->data = new_buf;
    action_list->capacity = new_capacity;
  }
}

bool action_list_needs_to_grow(const ActionList* action_list) {
  return action_list->capacity == 0 || action_list->length == action_list->capacity;
}

void action_list_push(ActionList* action_list, const Action* action) {
  if (action_list_needs_to_grow(action_list))
    action_list_grow(action_list);

  action_list->data[action_list->length++] = *action;
}

bool action_list_pop(ActionList* action_list, Action* action) {
  if (action_list->length == 0)
    return false;
  
  *action = action_list->data[--action_list->length];
  return true;
}

void action_list_clear(ActionList* action_list) {
  free_action_list(action_list);
  *action_list = new_action_list();
}