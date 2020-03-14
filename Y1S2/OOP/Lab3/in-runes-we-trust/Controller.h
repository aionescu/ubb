#ifndef CONTROLLER_H
#define CONTROLLER_H

#include <stdbool.h>
#include "Domain.h"
#include "Repo.h"

typedef struct {
  Repo repo;
} Controller;

// Initializes a new controller with an empty repository.
Controller newController();

// Frees any memory held by the controller.
void freeController(Controller* controller);

// Adds an ingredient to the controller, unless an ingredient with the same ID already exists.
// Returns: `false` if an ingredient with the same ID already exists, otherwise `true`.
bool controllerAddIngredient(Controller* controller, const Ingredient* ingredient);

// Attempts to remove the ingredient with the specified ID from the controller.
// Returns: `false` if no ingredient exists with the specified ID,
// otherwise `true`.
bool controllerUpdateIngredient(Controller* controller, const Ingredient* ingredient);

// Attempts to update the ingredient with the specified ID.
// Returns: `false` if no ingredient exists with the specified ID,
// otherwise `true`.
bool controllerRemoveIngredient(Controller* controller, int ingredientId);

// Attempts to undo the last performed action.
// Returns: `false` if no actions were performed, otherwise `true`.
bool controllerUndo(Controller* controller);

// Attempts to redo the last undone action.
// Returns: `false` if no actions were undone, otherwise `true`.
bool controllerRedo(Controller* controller);

// Returns a read-only view into the controller's data.
const Shelf* controllerData(const Controller* controller);

#endif