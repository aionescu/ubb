#ifndef CONTROLLER_H
#define CONTROLLER_H

#include <stdbool.h>
#include "Domain.h"
#include "Repo.h"

typedef struct {
  Repo repo;
} Controller;

bool controllerAddIngredient(Controller* controller, const Ingredient* ingredient);
bool controllerUpdateIngredient(Controller* controller, const Ingredient* ingredient);
bool controllerRemoveIngredient(Controller* controller, int ingredientId);

IngredientArray controllerData(const Controller* controller);

#endif