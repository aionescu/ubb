#include <stdio.h>
#include "UI.h"

UI ui;

int main() {
  while (true) {
    printf("> ");
    handleCommand(&ui);
  }
}