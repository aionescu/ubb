package tli.ast.expr;

import utils.collections.map.Map;

import tli.ast.Ident;
import tli.ast.type.Type;
import tli.ast.val.Val;

public interface Expr {
  public abstract Type typeCheck(Map<Ident, Type> sym);
  public abstract Val eval(Map<Ident, Val> sym);
}
