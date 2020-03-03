#ifndef __UI_H__
#define __UI_H__

#include "Controller.h"

typedef struct {
  Controller controller;
} UI;

void uiAddIngredient(UI* ui);
void uiUpdateIngredient(UI* ui);
void uiDeleteIngredient(UI* ui);

void uiListAll(UI* ui);
void uiListByIntendedUse(UI* ui);

void handleCommand(UI* ui);

#endif