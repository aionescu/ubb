package tli.repo;

import tli.ast.prog.ProgState;

public final class SingleStateRepository implements Repository {
  private ProgState _state;

  public SingleStateRepository(ProgState state) {
    _state = state;
  }

  @Override
  public ProgState state() {
    return _state;
  }

  @Override
  public void typeCheck() {
    _state.typeCheck();
  }

  @Override
  public void oneStep() {
    _state = _state.eval();
  }

  @Override
  public boolean done() {
    return _state.toDo.empty();
  }
}
