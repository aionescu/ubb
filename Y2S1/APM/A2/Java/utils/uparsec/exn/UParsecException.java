package utils.uparsec.exn;

public abstract class UParsecException extends RuntimeException {
  private final static long serialVersionUID = 1;

  @Override
  public abstract String getMessage();
}
