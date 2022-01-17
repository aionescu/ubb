#ifndef UI_H
#define UI_H

#include "Controller.h"

typedef struct {
  Controller controller;
} UI;

UI newUI();
void freeUI(UI* ui);

void uiAdd(UI* ui);
void uiUpdate(UI* ui);
void uiDelete(UI* ui);
void uiList(UI* ui);
void uiListState(UI* ui);

void uiUndo(UI* ui);
void uiRedo(UI* ui);

bool handleCommand(UI* ui);

void mainLoop(UI* ui);

#endif