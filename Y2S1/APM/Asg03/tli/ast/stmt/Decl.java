package tli.ast.stmt;

import utils.collections.map.Map;

import tli.ast.Ident;
import tli.ast.prog.ProgState;
import tli.ast.type.Type;
import tli.ast.val.Undefined;
import tli.exn.typeck.VariableAlreadyDeclaredException;

public final class Decl implements Stmt {
  private final Type _type;
  private final Ident _ident;

  public static Decl of(Type type, Ident ident) {
    return new Decl(type, ident);
  }

  public Decl(Type type, Ident ident) {
    _type = type;
    _ident = ident;
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
    return String.format("%s %s", _type, _ident);
  }
}
