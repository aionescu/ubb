#include <stdio.h>
#include <string.h>
#include "Test.h"
#include "UI.h"

int main() {
  runAllTests();
  
  UI ui = newUI();
  
  mainLoop(&ui);
  freeUI(&ui);
}