#ifndef __DOMAIN_H__
#define __DOMAIN_H__

#include "Utils.h"

typedef struct {
  int id;
  String state;
  String intendedUse;
  int potency;
} Ingredient;

String ingredientToString(Ingredient ingredient);

#endif
