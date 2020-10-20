package tli.exn;

public abstract class TLIException extends RuntimeException {
  private final static long serialVersionUID = 1;

  @Override
  public abstract String getMessage();
}
