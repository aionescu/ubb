package tli.exn.eval;

public final class EvaluationFinishedException extends EvalException {
  private final static long serialVersionUID = 1;

  @Override
  public String getMessage() {
    return "There are no more statements to execute.";
  }
}
