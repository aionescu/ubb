package tli.exn.eval;

public final class DivisionByZeroException extends EvalException {
  private final static long serialVersionUID = 1;

  @Override
  public String getMessage() {
    return "An attempt was made to divide by zero.";
  }
}
