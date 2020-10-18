package tli.exn.typeck;

import tli.ast.type.Type;

public final class TypeMismatchException extends TypeCheckerException {
  private final static long serialVersionUID = 1;

  private final Type _expected, _actual;

  public TypeMismatchException(Type expected, Type actual) {
    super();

    _expected = expected;
    _actual = actual;
  }

  @Override
  public String getMessage() {
    return String.format("Type mismatch: Expected type %s, but found type %s", _expected, _actual);
  }
}
