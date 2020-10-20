package utils.uparsec.exn;

public final class ParsingFailedException extends UParsecException {
  private final static long serialVersionUID = 1;

  @Override
  public String getMessage() {
    return "Parsing failed.";
  }
}
