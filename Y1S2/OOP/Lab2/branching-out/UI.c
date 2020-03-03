#include <stdio.h>
#include <string.h>
#include "UI.h"

void handleCommand(UI* ui) {
  char command[64];

  fgets(command, 64, stdin);

  sscanf(command, " ");

  if (!strcmp(command, "add")) {

  } else if (!strcmp(command, "update")) {

  } else if (!strcmp(command, "delete")) {

  } else if (!strcmp(command, "list")) {

  }
}