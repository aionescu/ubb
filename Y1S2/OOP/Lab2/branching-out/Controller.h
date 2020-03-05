#ifndef __CONTROLLER_H__
#define __CONTROLLER_H__

#include <stdbool.h>
#include "Domain.h"
#include "Repo.h"

typedef struct {
  Repo repo;
} Controller;

bool controllerAddIngredient(Controller* controller, const Ingredient* ingredient);
bool controllerUpdateIngredient(Controller* controller, const Ingredient* ingredient);
bool controllerRemoveIngredient(Controller* controller, int ingredientId);

void controllerListAll(const Controller* controller);
void controllerListByIntendedUse(const Controller* controller, const char* intendedUse);

#endif