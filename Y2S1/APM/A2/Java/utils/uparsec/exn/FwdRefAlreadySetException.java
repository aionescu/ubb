package utils.uparsec.exn;

public final class FwdRefAlreadySetException extends UParsecException {
  private final static long serialVersionUID = 1;

  @Override
  public String getMessage() {
    return "The forward reference has already been set.";
  }
}
