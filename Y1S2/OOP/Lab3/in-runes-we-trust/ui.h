#ifndef UI_H
#define UI_H

#include "controller.h"

typedef struct {
  Controller controller;
} Ui;

Ui new_ui();
void free_ui(Ui* ui);

void ui_add(Ui* ui);
void ui_update(Ui* ui);
void ui_delete(Ui* ui);
void ui_list(Ui* ui);
void ui_list_state(Ui* ui);

void ui_undo(Ui* ui);
void ui_redo(Ui* ui);

bool handle_command(Ui* ui);

#endif