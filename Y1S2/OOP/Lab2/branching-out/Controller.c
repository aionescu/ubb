#include <stdio.h>
#include <stdlib.h>
#include "Utils.h"
#include "Domain.h"
#include "Controller.h"

void listAll(Controller* controller) {
  for (int i = 0; i < controller->repo.ingredientCount; ++i) {
    String string = ingredientToString(controller->repo.ingredients[i]);

    printf("%s\n", string);
    free((void*)string);
  }
}

void listByIntendedUse(Controller* controller, Use intendedUse) {
  for (int i = 0; i < controller->repo.ingredientCount; ++i) {
    Ingredient ingredient = controller->repo.ingredients[i];

    if (ingredient.intendedUse == intendedUse) {
      String string = ingredientToString(ingredient);

      printf("%s\n", string);
      free((void*)string);
    }
  }
}
