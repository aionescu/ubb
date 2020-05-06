#ifndef DOMAIN_H
#define DOMAIN_H

#include <stdbool.h>

#define INGREDIENT_NAME_LENGTH 64

typedef struct {
  int id;
  char state[INGREDIENT_NAME_LENGTH + 1];
  char intendedUse[INGREDIENT_NAME_LENGTH + 1];
  int potency;
} Ingredient;

Ingredient zeroedIngredient();

// Constructs an ingredient from the specified components
// Note: If the length of `state` or `intendedUse` exceeds INGREDIENT_NAME_LENGTH, the excess
// bytes will not be copied into the new ingredient.
Ingredient newIngredient(int id, const char* state, const char* intendedUse, int potency);

typedef struct {
  int length, capacity;
  Ingredient* data;
} Shelf;

// Initializes a new shelf with 0 ingredients.
Shelf newShelf();

// Frees any memory held by the shelf.
void freeShelf(Shelf* shelf);

// Copies the contents of the shelf into a new shelf that is independent from the first.
// Note: It is the caller's responsibility to free the returned shelf.
Shelf copyShelf(const Shelf* shelf);

// Adds the specified ingredient to the end of the shelf.
// Post: The shelf now contains the new ingredient on its last position.
void shelfAddToEnd(Shelf* shelf, const Ingredient* ingredient);

bool shelfAddAt(Shelf* shelf, int index, const Ingredient* ingredient);

bool shelfRemoveFromEnd(Shelf* shelf);

// Removes the ingredient stored at the specified index.
// Returns: `false` if the index is out of range, otherwise `true`.
// Post: If the function returns `true`, then the element at the specified index was removed.
bool shelfRemoveAt(Shelf* shelf, int index);

// Iterates through the shelf, applying the specified function to each ingredient.
void shelfIter(const Shelf* shelf, void (*action)(const Ingredient* ingredient));

// Same behavior as `shelfIter`, but the function may modify the ingredient it is passed.
void shelfIterMut(Shelf* shelf, void (*action)(Ingredient* ingredient));

// Iterates through the shelf, providing access to each ingredient via the pointer specified by `iter`.
// This macro is meant to be used in place of a `for` loop, and so mimics its behavior.
#define SHELF_FOR(iter, shelf) for (const Ingredient* iter = (shelf)->data; iter < (shelf)->data + (shelf)->length; ++iter)

// Same behavior as `SHELF_FOR`, but the current ingredient may be modified.
#define SHELF_FOR_MUT(iter, shelf) for (Ingredient* iter = (shelf)->data; iter < (shelf)->data + (shelf)->length; ++iter)

#endif