#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "domain.h"
#include "controller.h"
#include "ui.h"

#define MAX_COMMAND_LENGTH 128
#define DELIM ", \n"

Ui new_ui() {
  Ui ui;
  ui.controller = new_controller();

  return ui;
}

void free_ui(Ui* ui) {
  free_controller(&ui->controller);
}

bool read_ingredient(Ingredient* destination) {
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

  strcpy(ingredient.intended_use, token);

  token = strtok(NULL, DELIM);

  if (!token)
    return false;

  ingredient.potency = atoi(token);

  *destination = ingredient;
  return true;
}

void ui_add(Ui* ui) {
  Ingredient ingredient;

  if (!read_ingredient(&ingredient)) {
    printf("Too few arguments.\n");
    return;
  }

  if (!controller_add_ingredient(&ui->controller, &ingredient))
    printf("No!\n");
}

void ui_update(Ui* ui) {
  Ingredient ingredient;

  if (!read_ingredient(&ingredient)) {
    printf("Too few arguments.\n");
    return;
  }

  if (!controller_update_ingredient(&ui->controller, &ingredient))
    printf("No!\n");
}

void ui_delete(Ui* ui) {
  const char* token = strtok(NULL, DELIM);

  if (!token) {
    printf("Too few arguments.\n");
    return;
  }

  int ingredient_id = atoi(token);

  if (!controller_remove_ingredient(&ui->controller, ingredient_id))
    printf("No!\n");
}

void print_ingredient(const Ingredient* ingredient) {
  printf("Ingredient #%d, State: %s, Intended use: %s, Potency: %d\n",
    ingredient->id,
    ingredient->state,
    ingredient->intended_use,
    ingredient->potency);
}

void ui_list_all(const Shelf* shelf) {
  shelf_iter(shelf, print_ingredient);
}

void ui_list_by_intended_use(const Shelf* shelf, const char* intended_use) {
  SHELF_FOR(ingredient, shelf)
    if (!strcmp(ingredient->intended_use, intended_use))
      print_ingredient(ingredient);
}

void ui_list_max_potency(const Shelf* shelf, int max_potency, int (*cmp)(const void* a, const void* b)) {
  Shelf new_shelf = copy_shelf(shelf);
  qsort(new_shelf.data, new_shelf.length, sizeof(Ingredient), cmp);

  SHELF_FOR(ingredient, shelf)
    if (ingredient->potency < max_potency)
      print_ingredient(ingredient);

  free_shelf(&new_shelf);
}

int cmp_asc(const void* a, const void* b) {
  const char* a_state = ((const Ingredient*)a)->state;
  const char* b_state = ((const Ingredient*)b)->state;

  return strcmp(a_state, b_state);
}

int cmp_desc(const void* a, const void* b) {
  const char* a_state = ((const Ingredient*)a)->state;
  const char* b_state = ((const Ingredient*)b)->state;

  return -strcmp(a_state, b_state);
}

void ui_list(Ui* ui) {
  const char* token = strtok(NULL, DELIM);

  const Shelf* shelf = controller_data(&ui->controller);

  if (!token) // list all
    ui_list_all(shelf);
  else {
    int max_potency = atoi(token);

    if (!max_potency) {
      ui_list_by_intended_use(shelf, token);
      return;
    }
    
    token = strtok(NULL, DELIM);

    if (token) {
      if (!strcmp(token, "desc"))
        ui_list_max_potency(shelf, max_potency, cmp_desc);
      else
        printf("Argument not recognized.\n");
    }
    else
      ui_list_max_potency(shelf, max_potency, cmp_asc);
  }
}

void ui_list_state(Ui* ui) {
  const char* state = strtok(NULL, DELIM);

  if (!state) {
    printf("Expected 1 argument.\n");
    return;
  }

  const Shelf* shelf = controller_data(&ui->controller);

  SHELF_FOR(ingredient, shelf)
    if (!strcmp(ingredient->state, state))
      print_ingredient(ingredient);
}

void ui_undo(Ui* ui) {
  if (!controller_undo(&ui->controller))
    printf("Nothing to undo.\n");
}

void ui_redo(Ui* ui) {
  if (!controller_redo(&ui->controller))
    printf("Nothing to redo.\n");
}

bool handle_command(Ui* ui) {
  char command[MAX_COMMAND_LENGTH];

  if (!fgets(command, MAX_COMMAND_LENGTH, stdin)) {
    printf("Error: Couldn't read input.\n");
    return true;
  }

  const char* token = strtok(command, DELIM);

  if (!strcmp(token, "exit"))
    return false;

  if (!strcmp(token, "add"))
    ui_add(ui);
  else if (!strcmp(token, "update"))
    ui_update(ui);
  else if (!strcmp(token, "delete"))
    ui_delete(ui);
  else if (!strcmp(token, "list"))
    ui_list(ui);
  else if (!strcmp(token, "list-state"))
    ui_list_state(ui);
  else if (!strcmp(token, "undo"))
    ui_undo(ui);
  else if (!strcmp(token, "redo"))
    ui_redo(ui);
  else
    printf("Command not recognized.\n");

  return true;
}