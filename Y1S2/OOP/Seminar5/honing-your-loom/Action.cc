#include "Action.hh"

Action::Action(Repo& repo) : _repo{repo} { }

AddAction::AddAction(Repo& repo, Task addedTask) : Action{repo}, _addedTask{addedTask} { }

void AddAction::undo() {
  _repo.remove(_addedTask.title());
}

void AddAction::redo() {
  _repo.add(_addedTask);
}

RemoveAction::RemoveAction(Repo& repo, Task removedTask) : Action{repo}, _removedTask{removedTask} { }

void RemoveAction::undo() {
  _repo.add(_removedTask);
}

void RemoveAction::redo() {
  _repo.remove(_removedTask.title());
}

UpdateAction::UpdateAction(Repo& repo, Task oldTask, Task newTask) : Action{repo}, _oldTask{oldTask}, _newTask{newTask} { }

void UpdateAction::undo() {
  _repo.update(_oldTask);
}

void UpdateAction::redo() {
  _repo.update(_newTask);
}