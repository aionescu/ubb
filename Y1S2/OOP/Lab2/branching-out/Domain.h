#ifndef DOMAIN_H
#define DOMAIN_H

#define INGREDIENT_NAME_LENGTH 64

typedef struct {
  int id;
  char state[INGREDIENT_NAME_LENGTH];
  char intendedUse[INGREDIENT_NAME_LENGTH];
  int potency;
} Ingredient;

void printIngredient(const Ingredient* ingredient);

#endif