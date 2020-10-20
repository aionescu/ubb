package tli.ast.stmt;

import utils.collections.list.List;
import utils.collections.map.Map;

import tli.ast.Ident;
import tli.ast.expr.Expr;
import tli.ast.prog.ProgState;
import tli.ast.type.Type;

public final class DeclAssign implements Stmt {
  private final Ident _ident;
  private final Type _type;
  private final Expr _expr;

  public static DeclAssign of(Ident ident, Type type, Expr expr) {
    return new DeclAssign(ident, type, expr);
  }

  public DeclAssign(Ident ident, Type type, Expr expr) {
    _ident = ident;
    _type = type;
    _expr = expr;
  }

  @Override
  public Map<Ident, Type> typeCheck(Map<Ident, Type> sym) {
    var sym2 = Decl.of(_ident, _type).typeCheck(sym);
    return Assign.of(_ident, _expr).typeCheck(sym2);
  }

  @Override
  public ProgState eval(ProgState prog) {
    return prog.withToDo(
      List.cons(Decl.of(_ident, _type),
        List.cons(Assign.of(_ident, _expr),
          prog.toDo)));
  }

  @Override
  public String toString() {
    return String.format("%s : %s <- %s", _ident, _type, _expr);
  }
}
