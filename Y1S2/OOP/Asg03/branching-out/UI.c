#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "Domain.h"
#include "UI.h"

#define MAX_COMMAND_LENGTH 128
#define DELIM ", \n"

bool readIngredient(Ingredient* destination) {
  Ingredient ingredient;
  const char* token = strtok(NULL, DELIM);
  
  if (!token)
    return false;
  
  ingredient.id = atoi(token);

  token = strtok(NULL, DELIM);

  if (!token)
    return false;

  strcpy(ingredient.state, token);

  token = strtok(NULL, DELIM);

  if (!token)
    return false;

  strcpy(ingredient.intendedUse, token);

  token = strtok(NULL, DELIM);

  if (!token)
    return false;

  ingredient.potency = atoi(token);

  *destination = ingredient;
  return true;
}

void uiAdd(UI* ui) {
  Ingredient ingredient;

  if (!readIngredient(&ingredient)) {
    printf("Too few arguments.\n");
    return;
  }

  if (!controllerAddIngredient(&ui->controller, &ingredient))
    printf("No!\n");
}

void uiUpdate(UI* ui) {
  Ingredient ingredient;

  if (!readIngredient(&ingredient)) {
    printf("Too few arguments.\n");
    return;
  }

  if (!controllerUpdateIngredient(&ui->controller, &ingredient))
    printf("No!\n");
}

void uiDelete(UI* ui) {
  Ingredient ingredient;

  const char* token = strtok(NULL, DELIM);

  if (!token) {
    printf("Too few arguments.\n");
    return;
  }

  int ingredientId = atoi(token);

  if (!controllerRemoveIngredient(&ui->controller, ingredientId))
    printf("No!\n");
}

void printIngredient(const Ingredient* ingredient) {
  printf("Ingredient #%d, State: %s, Intended use: %s, Potency: %d\n",
    ingredient->id,
    ingredient->state,
    ingredient->intendedUse,
    ingredient->potency);
}

void uiListAll(IngredientArray ingredients) {
  for (int i = 0; i < ingredients.length; ++i)
    printIngredient(&ingredients.data[i]);
}

void uiListByIntendedUse(IngredientArray ingredients, const char* intendedUse) {
  for (int i = 0; i < ingredients.length; ++i) {
    Ingredient ingredient = ingredients.data[i];

    if (!strcmp(ingredient.intendedUse, intendedUse))
      printIngredient(&ingredient);
  }
}

void uiList(UI* ui) {
  const char* token = strtok(NULL, DELIM);

  IngredientArray ingredients = controllerData(&ui->controller);

  if (!token) // list all
    uiListAll(ingredients);
  else
    uiListByIntendedUse(ingredients, token);
}

void uiExit(UI* ui) {
  exit(0);
}

void handleCommand(UI* ui) {
  char command[MAX_COMMAND_LENGTH];

  fgets(command, MAX_COMMAND_LENGTH, stdin);

  const char* token = strtok(command, DELIM);

  if (!strcmp(token, "add")) {
    uiAdd(ui);
  } else if (!strcmp(token, "update")) {
    uiUpdate(ui);
  } else if (!strcmp(token, "delete")) {
    uiDelete(ui);
  } else if (!strcmp(token, "list")) {
    uiList(ui);
  } else if (!strcmp(token, "exit")) {
    uiExit(ui);
  } else {
    printf("Command not recognized.\n");
  }
}