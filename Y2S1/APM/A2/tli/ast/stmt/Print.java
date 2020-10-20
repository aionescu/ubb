package tli.ast.stmt;

import tli.ast.Ident;
import tli.ast.expr.Expr;
import tli.ast.prog.ProgState;
import tli.ast.type.Type;
import utils.collections.list.List;
import utils.collections.map.Map;

public final class Print implements Stmt {
  private final Expr _expr;

  public static Print of(Expr expr) {
    return new Print(expr);
  }

  public Print(Expr expr) {
    _expr = expr;
  }

  @Override
  public Map<Ident, Type> typeCheck(Map<Ident, Type> sym) {
    _expr.typeCheck(sym);
    return sym;
  }

  @Override
  public ProgState eval(ProgState prog) {
    return prog.withOut(List.cons(_expr.eval(prog.sym).toString(), prog.out));
  }

  @Override
  public String toString() {
    return String.format("print %s", _expr);
  }
}
