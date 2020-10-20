package utils.uparsec.exn;

public final class ParserStuckException extends UParsecException {
  private final static long serialVersionUID = 1;

  @Override
  public String getMessage() {
    return "A parser succeeded without consuming any input when run inside one of the `many*` combinators. This exception was thrown to prevent an infinite loop.";
  }
}
