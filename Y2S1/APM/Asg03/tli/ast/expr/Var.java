package tli.ast.expr;

import utils.collections.map.Map;

import tli.ast.Ident;
import tli.ast.type.Type;
import tli.ast.val.Val;
import tli.exn.typeck.UndeclaredVariableException;

public final class Var implements Expr {
  private final Ident _ident;

  public Var(Ident ident) {
    _ident = ident;
  }

  @Override
  public Type typeCheck(Map<Ident, Type> sym) {
    var v = sym.lookup(_ident);

    if (!v.isPresent())
      throw new UndeclaredVariableException(_ident);

    return v.get();
  }

  @Override
  public Val eval(Map<Ident, Val> sym) {
    return sym.lookup(_ident).get();
  }

  @Override
  public String toString() {
    return _ident.toString();
  }
}
