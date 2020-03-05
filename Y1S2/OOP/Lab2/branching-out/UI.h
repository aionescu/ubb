#ifndef __UI_H__
#define __UI_H__

#include "Controller.h"

typedef struct {
  Controller controller;
} UI;

void uiAdd(UI* ui);
void uiUpdate(UI* ui);
void uiDelete(UI* ui);
void uiList(UI* ui);
void uiExit(UI* ui);

void handleCommand(UI* ui);

#endif