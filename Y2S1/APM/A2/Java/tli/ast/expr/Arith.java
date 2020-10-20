package tli.ast.expr;

import utils.collections.map.Map;

import tli.ast.Ident;
import tli.ast.type.Type;
import tli.ast.val.Int;
import tli.ast.val.Val;
import tli.ast.varstate.VarState;
import tli.exn.eval.DivisionByZeroException;

public final class Arith implements Expr {
  public static enum Op {
    ADD,
    SUB,
    MUL,
    DIV,
    REM;

    @Override
    public String toString() {
      return switch (this) {
        case ADD -> "+";
        case SUB -> "-";
        case MUL -> "*";
        case DIV -> "/";
        case REM -> "%";
      };
    }
  }

  private final Expr _lhs, _rhs;
  private final Op _op;

  public static Arith of(Expr lhs, Op op, Expr rhs) {
    return new Arith(lhs, op, rhs);
  }

  public Arith(Expr lhs, Op op, Expr rhs) {
    _lhs = lhs;
    _rhs = rhs;
    _op = op;
  }

  @Override
  public Type typeCheck(Map<Ident, Type> sym) {
    _lhs.typeCheck(sym).expect(Type.INT);
    _rhs.typeCheck(sym).expect(Type.INT);
    return Type.INT;
  }

  @Override
  public Val eval(Map<Ident, VarState> sym) {
    var lhs = ((Int)_lhs.eval(sym)).val;
    var rhs = ((Int)_rhs.eval(sym)).val;

    return new Int(switch (_op) {
      case ADD -> lhs + rhs;
      case SUB -> lhs - rhs;
      case MUL -> lhs * rhs;
      case DIV -> switch (rhs) {
        case 0 -> throw new DivisionByZeroException();
        default -> lhs / rhs;
      };
      case REM -> lhs % rhs;
    });
  }

  @Override
  public String toString() {
    return String.format("(%s %s %s)", _lhs, _op, _rhs);
  }
}
