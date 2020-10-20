package tli.ast.prog;

import utils.collections.list.List;
import utils.collections.map.Map;

import tli.ast.Ident;
import tli.ast.stmt.Stmt;
import tli.ast.varstate.VarState;

public final class ProgState {
  public final List<Stmt> toDo;
  public final Map<Ident, VarState> sym;
  public final List<String> out;

  public static final ProgState empty = new ProgState(List.nil(), Map.empty(), List.nil());

  private ProgState(List<Stmt> toDo, Map<Ident, VarState> sym, List<String> out) {
    this.toDo = toDo;
    this.sym = sym;
    this.out = out;
  }

  public ProgState withToDo(List<Stmt> toDo) {
    return new ProgState(toDo, this.sym, this.out);
  }

  public ProgState withSym(Map<Ident, VarState> sym) {
    return new ProgState(this.toDo, sym, this.out);
  }

  public ProgState withOut(List<String> out) {
    return new ProgState(this.toDo, this.sym, out);
  }

  public String output() {
    return out.reverse().unlines();
  }

  @Override
  public String toString() {
    return String.format("toDo = %s\nsym = %s\nout = %s\n", toDo, sym, out);
  }
}
