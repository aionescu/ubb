#ifndef DOMAIN_H
#define DOMAIN_H

#include <stdbool.h>

#define INGREDIENT_NAME_LENGTH 64

typedef struct {
  int id;
  char state[INGREDIENT_NAME_LENGTH + 1];
  char intended_use[INGREDIENT_NAME_LENGTH + 1];
  int potency;
} Ingredient;

Ingredient zeroed_ingredient();

// Constructs an ingredient from the specified components
// Note: If the length of `state` or `intended_use` exceeds INGREDIENT_NAME_LENGTH, the excess
// bytes will not be copied into the new ingredient.
Ingredient new_ingredient(int id, const char* state, const char* intended_use, int potency);

typedef struct {
  int length, capacity;
  Ingredient* data;
} Shelf;

// Initializes a new shelf with 0 ingredients.
Shelf new_shelf();

// Frees any memory held by the shelf.
void free_shelf(Shelf* shelf);

// Copies the contents of the shelf into a new shelf that is independent from the first.
// Note: It is the caller's responsibility to free the returned shelf.
Shelf copy_shelf(const Shelf* shelf);

// Adds the specified ingredient to the end of the shelf.
// Post: The shelf now contains the new ingredient on its last position.
void shelf_add_to_end(Shelf* shelf, const Ingredient* ingredient);

bool shelf_add_at(Shelf* shelf, int index, const Ingredient* ingredient);

bool shelf_remove_from_end(Shelf* shelf);

// Removes the ingredient stored at the specified index.
// Returns: `false` if the index is out of range, otherwise `true`.
// Post: If the function returns `true`, then the element at the specified index was removed.
bool shelf_remove_at(Shelf* shelf, int index);

// Iterates through the shelf, applying the specified function to each ingredient.
void shelf_iter(const Shelf* shelf, void (*action)(const Ingredient* ingredient));

// Same behavior as `shelf_iter`, but the function may modify the ingredient it is passed.
void shelf_iter_mut(Shelf* shelf, void (*action)(Ingredient* ingredient));

// Iterates through the shelf, providing access to each ingredient via the pointer specified by `iter`.
// This macro is meant to be used in place of a `for` loop, and so mimics its behavior.
#define SHELF_FOR(iter, shelf) for (const Ingredient* iter = (shelf)->data; iter < (shelf)->data + (shelf)->length; ++iter)

// Same behavior as `SHELF_FOR`, but the current ingredient may be modified.
#define SHELF_FOR_MUT(iter, shelf) for (Ingredient* iter = (shelf)->data; iter < (shelf)->data + (shelf)->length; ++iter)

#endif