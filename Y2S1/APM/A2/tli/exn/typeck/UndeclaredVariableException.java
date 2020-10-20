package tli.exn.typeck;

import tli.ast.Ident;

public final class UndeclaredVariableException extends TypeCheckerException {
  private final static long serialVersionUID = 1;

  private final Ident _ident;

  public UndeclaredVariableException(Ident ident) {
    super();

    _ident = ident;
  }

  @Override
  public String getMessage() {
    return String.format("Undefined variable: \"%s\"", _ident);
  }
}
