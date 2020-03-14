#ifndef CONTROLLER_H
#define CONTROLLER_H

#include <stdbool.h>
#include "domain.h"
#include "repo.h"

typedef struct {
  Repo repo;
} Controller;

// Initializes a new controller with an empty repository.
Controller new_controller();

// Frees any memory held by the controller.
void free_controller(Controller* controller);

// Adds an ingredient to the controller, unless an ingredient with the same ID already exists.
// Returns: `false` if an ingredient with the same ID already exists, otherwise `true`.
bool controller_add_ingredient(Controller* controller, const Ingredient* ingredient);

// Attempts to remove the ingredient with the specified ID from the controller.
// Returns: `false` if no ingredient exists with the specified ID,
// otherwise `true`.
bool controller_update_ingredient(Controller* controller, const Ingredient* ingredient);

// Attempts to update the ingredient with the specified ID.
// Returns: `false` if no ingredient exists with the specified ID,
// otherwise `true`.
bool controller_remove_ingredient(Controller* controller, int ingredient_id);

// Attempts to undo the last performed action.
// Returns: `false` if no actions were performed, otherwise `true`.
bool controller_undo(Controller* controller);

// Attempts to redo the last undone action.
// Returns: `false` if no actions were undone, otherwise `true`.
bool controller_redo(Controller* controller);

// Returns a read-only view into the controller's data.
const Shelf* controller_data(const Controller* controller);

#endif