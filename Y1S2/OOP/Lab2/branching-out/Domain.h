#ifndef __DOMAIN_H__
#define __DOMAIN_H__

#include "Utils.h"

typedef enum {
  Powder,
  Solid,
  Viscous,
  Liquid
} State;

Str stateToStr(State state);

typedef enum {
  Medicinal,
  Alchemical,
  Practical
} Use;

Str useToStr(Use use);

typedef struct {
  int id;
  State state;
  Use intendedUse;
  int potency;
} Ingredient;

String ingredientToString(Ingredient ingredient);

#endif
