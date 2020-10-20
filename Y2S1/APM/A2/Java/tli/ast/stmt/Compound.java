package tli.ast.stmt;

import tli.ast.Ident;
import tli.ast.prog.ProgState;
import tli.ast.type.Type;
import utils.collections.list.List;
import utils.collections.map.Map;

public final class Compound implements Stmt {
  private final Stmt _stmt1, _stmt2;

  public static Compound of(Stmt stmt1, Stmt stmt2) {
    return new Compound(stmt1, stmt2);
  }

  public Compound(Stmt stmt1, Stmt stmt2) {
    _stmt1 = stmt1;
    _stmt2 = stmt2;
  }

  @Override
  public Map<Ident, Type> typeCheck(Map<Ident, Type> sym) {
    return _stmt2.typeCheck(_stmt1.typeCheck(sym));
  }

  @Override
  public ProgState eval(ProgState prog) {
    return prog.withToDo(List.cons(_stmt1, List.cons(_stmt2, prog.toDo)));
  }

  @Override
  public String toString() {
    return String.format("%s; %s", _stmt1, _stmt2);
  }
}
