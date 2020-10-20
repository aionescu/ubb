package tli.ast.expr;

import utils.collections.map.Map;

import tli.ast.Ident;
import tli.ast.type.Type;
import tli.ast.val.Bool;
import tli.ast.val.Val;

public final class Logic implements Expr {
  public static enum Op {
    AND,
    OR;

    @Override
    public String toString() {
      return switch (this) {
        case AND -> "and";
        case OR -> "or";
      };
    }
  }

  private final Expr _lhs, _rhs;
  private final Op _op;

  public static Logic of(Expr lhs, Op op, Expr rhs) {
    return new Logic(lhs, op, rhs);
  }

  public Logic(Expr lhs, Op op, Expr rhs) {
    _lhs = lhs;
    _rhs = rhs;
    _op = op;
  }

  @Override
  public Type typeCheck(Map<Ident, Type> sym) {
    _lhs.typeCheck(sym).expect(Type.BOOL);
    _rhs.typeCheck(sym).expect(Type.BOOL);
    return Type.BOOL;
  }

  @Override
  public Val eval(Map<Ident, Val> sym) {
    var lhs = ((Bool)_lhs.eval(sym)).val;
    var rhs = ((Bool)_rhs.eval(sym)).val;

    return new Bool(switch (_op) {
      case AND -> lhs && rhs;
      case OR -> lhs || rhs;
    });
  }

  @Override
  public String toString() {
    return String.format("(%s %s %s)", _lhs, _op, _rhs);
  }
}
