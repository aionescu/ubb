#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "Domain.h"
#include "Controller.h"
#include "UI.h"

#define MAX_COMMAND_LENGTH 128
#define DELIM ", \n"

UI newUI() {
  UI ui;
  ui.controller = newController();

  return ui;
}

void freeUI(UI* ui) {
  freeController(&ui->controller);
}

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

void uiListAll(const Shelf* shelf) {
  shelfIter(shelf, printIngredient);
}

void uiListByIntendedUse(const Shelf* shelf, const char* intendedUse) {
  SHELF_FOR(ingredient, shelf)
    if (!strcmp(ingredient->intendedUse, intendedUse))
      printIngredient(ingredient);
}

void uiListMaxPotency(const Shelf* shelf, int maxPotency, int (*cmp)(const void* a, const void* b)) {
  Shelf newShelf = copyShelf(shelf);
  qsort(newShelf.data, newShelf.length, sizeof(Ingredient), cmp);

  SHELF_FOR(ingredient, shelf)
    if (ingredient->potency < maxPotency)
      printIngredient(ingredient);

  freeShelf(&newShelf);
}

int cmpAsc(const void* a, const void* b) {
  const char* aState = ((const Ingredient*)a)->state;
  const char* bState = ((const Ingredient*)b)->state;

  return strcmp(aState, bState);
}

int cmpDesc(const void* a, const void* b) {
  const char* aState = ((const Ingredient*)a)->state;
  const char* bState = ((const Ingredient*)b)->state;

  return -strcmp(aState, bState);
}

void uiList(UI* ui) {
  const char* token = strtok(NULL, DELIM);

  const Shelf* shelf = controllerData(&ui->controller);

  if (!token) // list all
    uiListAll(shelf);
  else {
    int maxPotency = atoi(token);

    if (!maxPotency) {
      uiListByIntendedUse(shelf, token);
      return;
    }
    
    token = strtok(NULL, DELIM);

    if (token) {
      if (!strcmp(token, "desc"))
        uiListMaxPotency(shelf, maxPotency, cmpDesc);
      else
        printf("Argument not recognized.\n");
    }
    else
      uiListMaxPotency(shelf, maxPotency, cmpAsc);
  }
}

void uiListState(UI* ui) {
  const char* state = strtok(NULL, DELIM);

  if (!state) {
    printf("Expected 1 argument.\n");
    return;
  }

  const Shelf* shelf = controllerData(&ui->controller);

  SHELF_FOR(ingredient, shelf)
    if (!strcmp(ingredient->state, state))
      printIngredient(ingredient);
}

void uiUndo(UI* ui) {
  if (!controllerUndo(&ui->controller))
    printf("Nothing to undo.\n");
}

void uiRedo(UI* ui) {
  if (!controllerRedo(&ui->controller))
    printf("Nothing to redo.\n");
}

bool handleCommand(UI* ui) {
  char command[MAX_COMMAND_LENGTH];

  if (!fgets(command, MAX_COMMAND_LENGTH, stdin)) {
    printf("Error: Couldn't read input.\n");
    return true;
  }

  const char* token = strtok(command, DELIM);

  if (!strcmp(token, "exit"))
    return false;

  if (!strcmp(token, "add"))
    uiAdd(ui);
  else if (!strcmp(token, "update"))
    uiUpdate(ui);
  else if (!strcmp(token, "delete"))
    uiDelete(ui);
  else if (!strcmp(token, "list"))
    uiList(ui);
  else if (!strcmp(token, "list-state"))
    uiListState(ui);
  else if (!strcmp(token, "undo"))
    uiUndo(ui);
  else if (!strcmp(token, "redo"))
    uiRedo(ui);
  else
    printf("Command not recognized.\n");

  return true;
}