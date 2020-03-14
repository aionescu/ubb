#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "test.h"
#include "domain.h"
#include "action.h"
#include "repo.h"
#include "controller.h"

void test__new_ingredient__does_not_exceed_max_length() {
  char data[101];
  memset(data, 'a', 100);
  data[100] = '\0';

  Ingredient ingredient = new_ingredient(1, data, data, 100);

  assert(strlen(ingredient.state) == INGREDIENT_NAME_LENGTH);
  assert(strlen(ingredient.intended_use) == INGREDIENT_NAME_LENGTH);
}

void test__new_shelf__is_empty() {
  Shelf shelf = new_shelf();

  assert(shelf.length == 0);
  assert(shelf.capacity == 0);

  free_shelf(&shelf);
}

void test__shelf_add_to_end__shelf_grows() {
  Shelf shelf = new_shelf();
  Ingredient ingredient = zeroed_ingredient();

  shelf_add_to_end(&shelf, &ingredient);
  assert(shelf.length == 1);

  shelf_add_to_end(&shelf, &ingredient);
  assert(shelf.length == 2);

  free_shelf(&shelf);
}

void test__shelf_remove_from_end__shelf_shrinks() {
  Shelf shelf = new_shelf();
  Ingredient ingredient = zeroed_ingredient();

  shelf_add_to_end(&shelf, &ingredient);
  shelf_add_to_end(&shelf, &ingredient);

  assert(shelf_remove_from_end(&shelf));
  assert(shelf.length == 1);

  assert(shelf_remove_from_end(&shelf));
  assert(shelf.length == 0);

  assert(!shelf_remove_from_end(&shelf));

  free_shelf(&shelf);
}

void test__new_action_list__is_empty() {
  ActionList action_list = new_action_list();

  assert(action_list.length == 0);

  free_action_list(&action_list);
}

void test__action_list_push__grows() {
  ActionList action_list = new_action_list();
  Ingredient ingredient = zeroed_ingredient();
  Action action = new_add(&ingredient);

  action_list_push(&action_list, &action);
  assert(action_list.length == 1);

  action_list_push(&action_list, &action);
  assert(action_list.length == 2);

  free_action_list(&action_list);
}

void test__action_list_pop__shrinks() {
  ActionList action_list = new_action_list();
  Ingredient ingredient = zeroed_ingredient();
  Action action = new_add(&ingredient);

  action_list_push(&action_list, &action);
  action_list_push(&action_list, &action);

  assert(action_list_pop(&action_list, &action));
  assert(action_list.length == 1);

  assert(action_list_pop(&action_list, &action));
  assert(action_list.length == 0);

  assert(!action_list_pop(&action_list, &action));

  free_action_list(&action_list);
}

void test__repo_add_ingredient__fails_when_duplicate_added() {
  Repo repo = new_repo();
  Ingredient ingredient = zeroed_ingredient();

  assert(repo_add_ingredient(&repo, &ingredient));
  assert(!repo_add_ingredient(&repo, &ingredient));

  free_repo(&repo);
}

void test__repo_update_ingredient__fails_on_inexistent_ingredient() {
  Repo repo = new_repo();
  Ingredient ingredient = zeroed_ingredient();

  assert(!repo_update_ingredient(&repo, &ingredient));

  free_repo(&repo);
}

void test__repo_remove_ingredient__fails_on_inexistent_ingredient() {
  Repo repo = new_repo();
  Ingredient ingredient = zeroed_ingredient();

  repo_add_ingredient(&repo, &ingredient);

  assert(repo_remove_ingredient(&repo, ingredient.id));
  assert(!repo_remove_ingredient(&repo, ingredient.id));

  free_repo(&repo);
}

void test__repo_undo__fails_when_nothing_to_undo() {
  Repo repo = new_repo();

  assert(!repo_undo(&repo));

  free_repo(&repo);
}

void test__repo_redo__fails_when_nothing_to_redo() {
  Repo repo = new_repo();

  assert(!repo_redo(&repo));

  free_repo(&repo);
}

void run_all_tests() {
  test__new_ingredient__does_not_exceed_max_length();

  test__new_shelf__is_empty();
  test__shelf_add_to_end__shelf_grows();
  test__shelf_remove_from_end__shelf_shrinks();

  test__new_action_list__is_empty();
  test__action_list_push__grows();
  test__action_list_pop__shrinks();

  test__repo_add_ingredient__fails_when_duplicate_added();
  test__repo_update_ingredient__fails_on_inexistent_ingredient();
  test__repo_remove_ingredient__fails_on_inexistent_ingredient();

  test__repo_undo__fails_when_nothing_to_undo();
  test__repo_redo__fails_when_nothing_to_redo();
  
  printf("All tests passed.\n");
}