#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "Test.h"
#include "Domain.h"
#include "Action.h"
#include "Repo.h"
#include "Controller.h"

void test_newIngredient_doesNotExceedMaxLength() {
  char data[101];
  memset(data, 'a', 100);
  data[100] = '\0';

  Ingredient ingredient = newIngredient(1, data, data, 100);

  assert(strlen(ingredient.state) == INGREDIENT_NAME_LENGTH);
  assert(strlen(ingredient.intendedUse) == INGREDIENT_NAME_LENGTH);
}

void test_newShelf_isEmpty() {
  Shelf shelf = newShelf();

  assert(shelf.length == 0);
  assert(shelf.capacity == 0);

  freeShelf(&shelf);
}

void test_shelfAddToEnd_shelfGrows() {
  Shelf shelf = newShelf();
  Ingredient ingredient = zeroedIngredient();

  shelfAddToEnd(&shelf, &ingredient);
  assert(shelf.length == 1);

  shelfAddToEnd(&shelf, &ingredient);
  assert(shelf.length == 2);

  freeShelf(&shelf);
}

void test_shelfRemoveFromEnd_shelfShrinks() {
  Shelf shelf = newShelf();
  Ingredient ingredient = zeroedIngredient();

  shelfAddToEnd(&shelf, &ingredient);
  shelfAddToEnd(&shelf, &ingredient);

  assert(shelfRemoveFromEnd(&shelf));
  assert(shelf.length == 1);

  assert(shelfRemoveFromEnd(&shelf));
  assert(shelf.length == 0);

  assert(!shelfRemoveFromEnd(&shelf));

  freeShelf(&shelf);
}

void test_newActionList_isEmpty() {
  ActionList actionList = newActionList();

  assert(actionList.length == 0);

  freeActionList(&actionList);
}

void test_actionListPush_listGrows() {
  ActionList actionList = newActionList();
  Ingredient ingredient = zeroedIngredient();
  Action action = newAdd(&ingredient);

  actionListPush(&actionList, &action);
  assert(actionList.length == 1);

  actionListPush(&actionList, &action);
  assert(actionList.length == 2);

  freeActionList(&actionList);
}

void test_actionListPop_listShrinks() {
  ActionList actionList = newActionList();
  Ingredient ingredient = zeroedIngredient();
  Action action = newAdd(&ingredient);

  actionListPush(&actionList, &action);
  actionListPush(&actionList, &action);

  assert(actionListPop(&actionList, &action));
  assert(actionList.length == 1);

  assert(actionListPop(&actionList, &action));
  assert(actionList.length == 0);

  assert(!actionListPop(&actionList, &action));

  freeActionList(&actionList);
}

void test_repoAddIngredient_failsWhenDuplicateAdded() {
  Repo repo = newRepo();
  Ingredient ingredient = zeroedIngredient();

  assert(repoAddIngredient(&repo, &ingredient));
  assert(!repoAddIngredient(&repo, &ingredient));

  freeRepo(&repo);
}

void test_repoUpdateIngredient_failsOnInexistentIngredient() {
  Repo repo = newRepo();
  Ingredient ingredient = zeroedIngredient();

  assert(!repoUpdateIngredient(&repo, &ingredient));

  freeRepo(&repo);
}

void test_repoRemoveIngredient_failsOnInexistentIngredient() {
  Repo repo = newRepo();
  Ingredient ingredient = zeroedIngredient();

  repoAddIngredient(&repo, &ingredient);

  assert(repoRemoveIngredient(&repo, ingredient.id));
  assert(!repoRemoveIngredient(&repo, ingredient.id));

  freeRepo(&repo);
}

void test_repoUndo_failsWhenNothingToUndo() {
  Repo repo = newRepo();

  assert(!repoUndo(&repo));

  freeRepo(&repo);
}

void test_repoRedo_failsWhenNothingToRedo() {
  Repo repo = newRepo();

  assert(!repoRedo(&repo));

  freeRepo(&repo);
}

void runAllTests() {
  test_newIngredient_doesNotExceedMaxLength();

  test_newShelf_isEmpty();
  test_shelfAddToEnd_shelfGrows();
  test_shelfRemoveFromEnd_shelfShrinks();

  test_newActionList_isEmpty();
  test_actionListPush_listGrows();
  test_actionListPop_listShrinks();

  test_repoAddIngredient_failsWhenDuplicateAdded();
  test_repoUpdateIngredient_failsOnInexistentIngredient();
  test_repoRemoveIngredient_failsOnInexistentIngredient();

  test_repoUndo_failsWhenNothingToUndo();
  test_repoRedo_failsWhenNothingToRedo();
  
  printf("All tests passed.\n");
}