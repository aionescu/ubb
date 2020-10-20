package tli.ast.stmt;

import utils.collections.map.Map;

import tli.ast.Ident;
import tli.ast.prog.ProgState;
import tli.ast.type.Type;
import tli.ast.varstate.Undefined;
import tli.exn.typeck.VariableAlreadyDeclaredException;

public final class Decl implements Stmt {
  private final Ident _ident;
  private final Type _type;

  public static Decl of(Ident ident, Type type) {
    return new Decl(ident, type);
  }

  public Decl(Ident ident, Type type) {
    _ident = ident;
    _type = type;
  }

  @Override
  public Map<Ident, Type> typeCheck(Map<Ident, Type> sym) {
    if (sym.lookup(_ident).isPresent())
      throw new VariableAlreadyDeclaredException(_ident);

    return sym.insert(_ident, _type);
  }

  @Override
  public ProgState eval(ProgState prog) {
    return prog.withSym(prog.sym.insert(_ident, Undefined.value));
  }

  @Override
  public String toString() {
    return String.format("%s : %s", _ident, _type);
  }
}
