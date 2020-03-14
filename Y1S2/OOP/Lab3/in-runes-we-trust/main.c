#include <stdio.h>
#include <string.h>
#include "test.h"
#include "ui.h"

int main() {
  run_all_tests();
  
  Ui ui = new_ui();

  do {
    printf("> ");
  } while (handle_command(&ui));

  free_ui(&ui);
}