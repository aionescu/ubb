package tli.ast.prog;

import utils.collections.list.List;
import utils.collections.map.Map;

import tli.ast.Ident;
import tli.ast.stmt.Stmt;
import tli.ast.type.Type;
import tli.ast.val.Val;
import tli.exn.eval.EvaluationFinishedException;

public final class ProgState {
  public final List<Stmt> toDo;
  public final Map<Ident, Val> sym;
  public final List<String> out;

  public static final ProgState empty = new ProgState(List.nil(), Map.empty(), List.nil());

  private ProgState(List<Stmt> toDo, Map<Ident, Val> sym, List<String> out) {
    this.toDo = toDo;
    this.sym = sym;
    this.out = out;
  }

  public ProgState withToDo(List<Stmt> toDo) {
    return new ProgState(toDo, this.sym, this.out);
  }

  public ProgState withSym(Map<Ident, Val> sym) {
    return new ProgState(this.toDo, sym, this.out);
  }

  public ProgState withOut(List<String> out) {
    return new ProgState(this.toDo, this.sym, out);
  }

  public String output() {
    return out.reverse().foldl(String::concat, "");
  }

  @Override
  public String toString() {
    return String.format("ProgState {\n  toDo = %s,\n  sym = %s,\n  out = %s\n}", toDo, sym, out);
  }
}
