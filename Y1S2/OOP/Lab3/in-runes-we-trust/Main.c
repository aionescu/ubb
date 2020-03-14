#include <stdio.h>
#include <string.h>
#include "Test.h"
#include "UI.h"

int main() {
  runAllTests();
  
  UI ui = newUI();

  do {
    printf("> ");
  } while (handleCommand(&ui));

  freeUI(&ui);
}