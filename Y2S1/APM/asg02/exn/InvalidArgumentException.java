package asg02.exn;

public class InvalidArgumentException extends RuntimeException {
  private static final long serialVersionUID = 1;

  public InvalidArgumentException() {
    super();
  }

  public InvalidArgumentException(String message) {
    super(message);
  }

  public InvalidArgumentException(String message, Throwable err) {
    super(message, err);
  }
}
