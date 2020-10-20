package tli.ast.stmt;

import tli.ast.Ident;
import tli.ast.expr.Expr;
import tli.ast.val.Bool;
import tli.ast.prog.ProgState;
import tli.ast.type.Type;
import utils.collections.list.List;
import utils.collections.map.Map;

public final class While implements Stmt {
  private final Expr _cond;
  private final Stmt _body;

  public static While of(Expr cond, Stmt body) {
    return new While(cond, body);
  }

  public While(Expr cond, Stmt body) {
    _cond = cond;
    _body = body;
  }

  @Override
  public Map<Ident, Type> typeCheck(Map<Ident, Type> sym) {
    _cond.typeCheck(sym).expect(Type.BOOL);
    _body.typeCheck(sym);
    return sym;
  }

  @Override
  public ProgState eval(ProgState prog) {
    var v = ((Bool)_cond.eval(prog.sym)).val;
    var toDo = v ? List.cons(_body, List.cons(this, prog.toDo)) : prog.toDo;

    return prog.withToDo(toDo);
  }

  @Override
  public String toString() {
    return String.format("while %s { %s }", _cond, _body);
  }
}
