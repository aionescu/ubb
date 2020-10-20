package tli.ast.stmt;

import utils.collections.map.Map;

import tli.ast.Ident;
import tli.ast.prog.ProgState;
import tli.ast.type.Type;

public final class Nop implements Stmt {
  public static final Nop nop = new Nop();

  private Nop() { }

  @Override
  public Map<Ident, Type> typeCheck(Map<Ident, Type> sym) {
    return sym;
  }

  @Override
  public ProgState eval(ProgState prog) {
    return prog;
  }

  @Override
  public String toString() {
    return "";
  }
}
