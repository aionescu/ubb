#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include "domain.h"

Ingredient zeroed_ingredient() {
  Ingredient ingredient;
  memset(&ingredient, 0, sizeof(Ingredient));

  return ingredient;
}

#define MIN(a, b) (((a) < (b)) ? (a) : (b))

Ingredient new_ingredient(int id, const char* state, const char* intended_use, int potency) {
  Ingredient ingredient;

  ingredient.id = id;

  memset(ingredient.state, 0, INGREDIENT_NAME_LENGTH + 1);
  memcpy(ingredient.state, state, MIN(strlen(state), INGREDIENT_NAME_LENGTH));

  memset(ingredient.intended_use, 0, INGREDIENT_NAME_LENGTH + 1);
  memcpy(ingredient.intended_use, intended_use, MIN(strlen(intended_use), INGREDIENT_NAME_LENGTH));

  ingredient.potency = potency;

  return ingredient;
}

Shelf new_shelf() {
  Shelf shelf;
  shelf.length = shelf.capacity = 0;
  shelf.data = NULL;

  return shelf;
}

void free_shelf(Shelf* shelf) {
  shelf->length = shelf->capacity = 0;

  if (shelf->data) {
    free(shelf->data);
    shelf->data = NULL;
  }
}

Shelf copy_shelf(const Shelf* shelf) {
  Shelf new_shelf = *shelf;

  Ingredient* new_buf = malloc(shelf->capacity * sizeof(Ingredient));
  memcpy(new_buf, shelf->data, shelf->length * sizeof(Ingredient));

  new_shelf.data = new_buf;
  return new_shelf;
}

void shelf_grow(Shelf* shelf) {
  if (shelf->capacity == 0) {
    if (shelf->data)
      free(shelf->data);
      
    shelf->data = malloc(sizeof(Ingredient));
    shelf->capacity = 1;
  } else {
    int new_capacity = shelf->capacity * 2;
    Ingredient* new_buf = malloc(new_capacity * sizeof(Ingredient));
    memcpy(new_buf, shelf->data, shelf->length * sizeof(Ingredient));

    free(shelf->data);
    shelf->data = new_buf;
    shelf->capacity = new_capacity;
  }
}

bool shelf_needs_to_grow(const Shelf* shelf) {
  return shelf->capacity == 0 || shelf->length == shelf->capacity;
}

void shelf_add_to_end(Shelf* shelf, const Ingredient* ingredient) {
  if (shelf_needs_to_grow(shelf))
    shelf_grow(shelf);

  shelf->data[shelf->length++] = *ingredient;
}

bool shelf_add_at(Shelf* shelf, int index, const Ingredient* ingredient) {
  if (index < 0 || index >= shelf->length)
    return false;

  if (shelf_needs_to_grow(shelf))
    shelf_grow(shelf);

  for (int i = shelf->length; i > index; --i) {
    shelf->data[i] = shelf->data[i - 1];
  }

  shelf->data[index] = *ingredient;
  return true;
}

bool shelf_remove_from_end(Shelf* shelf) {
  if (shelf->length == 0)
    return false;

  --shelf->length;
  return true;
}

bool shelf_remove_at(Shelf* shelf, int index) {
  if (index < 0 || index >= shelf->length)
    return false;

  for (int i = index; i < shelf->length - 1; ++i)
    shelf->data[i] = shelf->data[i + 1];

  --shelf->length;
  return true;
}

void shelf_iter(const Shelf* shelf, void (*action)(const Ingredient* ingredient)) {
  for (int i = 0; i < shelf->length; ++i)
    action(&shelf->data[i]);
}

void shelf_iter_mut(Shelf* shelf, void (*action)(Ingredient* ingredient)) {
  for (int i = 0; i < shelf->length; ++i)
    action(&shelf->data[i]);
}