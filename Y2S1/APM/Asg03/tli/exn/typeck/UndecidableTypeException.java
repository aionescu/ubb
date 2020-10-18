package tli.exn.typeck;

public final class UndecidableTypeException extends TypeCheckerException {
  private final static long serialVersionUID = 1;

  @Override
  public String getMessage() {
    return "Not enough information was provided to conduct type checking.";
  }
}
