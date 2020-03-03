#ifndef __CONTROLLER_H__
#define __CONTROLLER_H__

#include "Utils.h"
#include "Domain.h"
#include "Repo.h"

typedef struct {
  Repo repo;
} Controller;

void listAll(Controller* controller);
void listByIntendedUse(Controller* controller, Str intendedUse);

#endif
