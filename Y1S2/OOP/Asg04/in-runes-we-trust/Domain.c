#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include "Domain.h"

Ingredient zeroedIngredient() {
  Ingredient ingredient;
  memset(&ingredient, 0, sizeof(Ingredient));

  return ingredient;
}

#define MIN(a, b) (((a) < (b)) ? (a) : (b))

Ingredient newIngredient(int id, const char* state, const char* intendedUse, int potency) {
  Ingredient ingredient;

  ingredient.id = id;

  memset(ingredient.state, 0, INGREDIENT_NAME_LENGTH + 1);
  memcpy(ingredient.state, state, MIN(strlen(state), INGREDIENT_NAME_LENGTH));

  memset(ingredient.intendedUse, 0, INGREDIENT_NAME_LENGTH + 1);
  memcpy(ingredient.intendedUse, intendedUse, MIN(strlen(intendedUse), INGREDIENT_NAME_LENGTH));

  ingredient.potency = potency;

  return ingredient;
}

Shelf newShelf() {
  Shelf shelf;
  shelf.length = shelf.capacity = 0;
  shelf.data = NULL;

  return shelf;
}

void freeShelf(Shelf* shelf) {
  shelf->length = shelf->capacity = 0;

  if (shelf->data) {
    free(shelf->data);
    shelf->data = NULL;
  }
}

Shelf copyShelf(const Shelf* shelf) {
  Shelf newShelf = *shelf;

  Ingredient* newBuffer = malloc(shelf->capacity * sizeof(Ingredient));
  memcpy(newBuffer, shelf->data, shelf->length * sizeof(Ingredient));

  newShelf.data = newBuffer;
  return newShelf;
}

void shelfGrow(Shelf* shelf) {
  if (shelf->capacity == 0) {
    if (shelf->data)
      free(shelf->data);
      
    shelf->data = malloc(sizeof(Ingredient));
    shelf->capacity = 1;
  } else {
    int newCapacity = shelf->capacity * 2;
    Ingredient* newBuffer = malloc(newCapacity * sizeof(Ingredient));
    memcpy(newBuffer, shelf->data, shelf->length * sizeof(Ingredient));

    free(shelf->data);
    shelf->data = newBuffer;
    shelf->capacity = newCapacity;
  }
}

bool shelfNeedsToGrow(const Shelf* shelf) {
  return shelf->capacity == 0 || shelf->length == shelf->capacity;
}

void shelfAddToEnd(Shelf* shelf, const Ingredient* ingredient) {
  if (shelfNeedsToGrow(shelf))
    shelfGrow(shelf);

  shelf->data[shelf->length++] = *ingredient;
}

bool shelfAddAt(Shelf* shelf, int index, const Ingredient* ingredient) {
  if (index < 0 || index >= shelf->length)
    return false;

  if (shelfNeedsToGrow(shelf))
    shelfGrow(shelf);

  for (int i = shelf->length; i > index; --i) {
    shelf->data[i] = shelf->data[i - 1];
  }

  shelf->data[index] = *ingredient;
  return true;
}

bool shelfRemoveFromEnd(Shelf* shelf) {
  if (shelf->length == 0)
    return false;

  --shelf->length;
  return true;
}

bool shelfRemoveAt(Shelf* shelf, int index) {
  if (index < 0 || index >= shelf->length)
    return false;

  for (int i = index; i < shelf->length - 1; ++i)
    shelf->data[i] = shelf->data[i + 1];

  --shelf->length;
  return true;
}

void shelfIter(const Shelf* shelf, void (*action)(const Ingredient* ingredient)) {
  for (int i = 0; i < shelf->length; ++i)
    action(&shelf->data[i]);
}

void shelfIterMut(Shelf* shelf, void (*action)(Ingredient* ingredient)) {
  for (int i = 0; i < shelf->length; ++i)
    action(&shelf->data[i]);
}