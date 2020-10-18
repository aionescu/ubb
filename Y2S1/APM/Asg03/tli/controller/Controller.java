package tli.controller;

import tli.ast.prog.ProgState;
import tli.repo.Repository;

public final class Controller {
  Repository _repo;

  public Controller(Repository repo) {
    _repo = repo;
  }

  public ProgState state() {
    return _repo.state();
  }

  public void typeCheck() {
    _repo.typeCheck();
  }

  public void oneStep() {
    _repo.oneStep();
  }

  public boolean done() {
    return _repo.done();
  }

  public void allSteps() {
    while (!done())
      oneStep();
  }
}
