package tli.ast.stmt;

import tli.ast.Ident;
import tli.ast.expr.Expr;
import tli.ast.prog.ProgState;
import tli.ast.type.Type;
import tli.exn.typeck.UndeclaredVariableException;
import utils.collections.map.Map;

public final class Assign implements Stmt {
  private final Ident _ident;
  private final Expr _expr;

  public Assign(Ident ident, Expr expr) {
    _ident = ident;
    _expr = expr;
  }

  @Override
  public Map<Ident, Type> typeCheck(Map<Ident, Type> sym) {
    var v = sym.lookup(_ident);

    if (!v.isPresent())
      throw new UndeclaredVariableException(_ident);

      _expr.typeCheck(sym).expect(v.get());
    return sym;
  }

  @Override
  public ProgState eval(ProgState prog) {
    return prog.withSym(prog.sym.insert(_ident, _expr.eval(prog.sym)));
  }

  @Override
  public String toString() {
    return String.format("%s = %s", _ident, _expr);
  }
}
