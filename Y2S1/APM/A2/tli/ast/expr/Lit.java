package tli.ast.expr;

import tli.ast.type.Type;
import tli.ast.val.*;

import utils.collections.map.Map;

import tli.ast.Ident;
import tli.exn.typeck.UndecidableTypeException;

public final class Lit implements Expr {
  private final Val _val;

  public static Lit of(Val val) {
    return new Lit(val);
  }

  public Lit(Val val) {
    _val = val;
  }

  @Override
  public Type typeCheck(Map<Ident, Type> sym) {
    if (_val instanceof Int)
      return Type.INT;
    else if (_val instanceof Bool)
      return Type.BOOL;
    else
      throw new UndecidableTypeException();
  }

  @Override
  public Val eval(Map<Ident, Val> sym) {
    return _val;
  }

  @Override
  public String toString() {
    return _val.toString();
  }
}
