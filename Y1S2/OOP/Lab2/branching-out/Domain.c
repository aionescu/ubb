#include <stdio.h>
#include "Domain.h"

void printIngredient(const Ingredient* ingredient) {
  printf("Ingredient #%d, State: %s, Intended use: %s, Potency: %d\n",
    ingredient->id,
    ingredient->state,
    ingredient->intendedUse,
    ingredient->potency);
}