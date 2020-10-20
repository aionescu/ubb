package tli.ast.stmt;

import tli.ast.Ident;
import tli.ast.expr.Expr;
import tli.ast.val.Bool;
import tli.ast.prog.ProgState;
import tli.ast.type.Type;
import utils.collections.list.List;
import utils.collections.map.Map;

public final class If implements Stmt {
  private final Expr _cond;
  private final Stmt _then, _else;

  public static If of(Expr cond, Stmt then, Stmt else_) {
    return new If(cond, then, else_);
  }

  public If(Expr cond, Stmt then, Stmt else_) {
    _cond = cond;
    _then = then;
    _else = else_;
  }

  @Override
  public Map<Ident, Type> typeCheck(Map<Ident, Type> sym) {
    _cond.typeCheck(sym).expect(Type.BOOL);
    _then.typeCheck(sym);
    _else.typeCheck(sym);
    return sym;
  }

  @Override
  public ProgState eval(ProgState prog) {
    var v = ((Bool)_cond.eval(prog.sym)).val;
    var block = v ? _then : _else;

    return prog.withToDo(List.cons(block, prog.toDo));
  }

  @Override
  public String toString() {
    return String.format("if %s { %s } else { %s }", _cond, _then, _else);
  }
}
