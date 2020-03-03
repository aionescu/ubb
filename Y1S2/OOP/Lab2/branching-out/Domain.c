#include <stdio.h>
#include <stdlib.h>
#include "Utils.h"
#include "Domain.h"

String ingredientToString(Ingredient ingredient) {
  char* buffer = malloc(512);

  sprintf(buffer,
    "Ingredient #%d, State: %s, Intended use: %s, Potency: %d",
    ingredient.id,
    ingredient.state,
    ingredient.intendedUse,
    ingredient.potency);

  return buffer;
}
