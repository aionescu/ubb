package tli.repo;

import tli.ast.Ident;
import tli.ast.prog.ProgState;
import tli.ast.type.Type;
import tli.exn.eval.EvaluationFinishedException;
import utils.collections.map.Map;

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
    _state.toDo.foldl((sym, stmt) -> stmt.typeCheck(sym), Map.<Ident, Type>empty());
  }

  @Override
  public void oneStep() {
    _state = _state.toDo.match(
      () -> { throw new EvaluationFinishedException(); },
      (stmt, toDo) -> stmt.eval(_state.withToDo(toDo)));
  }

  @Override
  public boolean done() {
    return _state.toDo.empty();
  }
}
