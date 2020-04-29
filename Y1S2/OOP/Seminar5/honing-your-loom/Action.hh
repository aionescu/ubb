#ifndef ACTION_HH
#define ACTION_HH

#include "Repo.hh"

class Action {
protected:
  Repo& _repo;

public:
  virtual ~Action() { }

  Action(Repo& repo);

  virtual void undo() = 0;
  virtual void redo() = 0;
};

class AddAction : public Action {
  Task _addedTask;

public:
  AddAction(Repo& repo, Task addedTask);

  void undo() override;
  void redo() override;
};

class RemoveAction : public Action {
  Task _removedTask;

public:
  RemoveAction(Repo& repo, Task removedTask);

  void undo() override;
  void redo() override;
};

class UpdateAction : public Action {
  Task _oldTask;
  Task _newTask;

public:
  UpdateAction(Repo& repo, Task oldTask, Task newTask);

  void undo() override;
  void redo() override;
};

#endif