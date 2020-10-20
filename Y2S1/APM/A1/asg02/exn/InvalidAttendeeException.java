package asg02.exn;

public class InvalidAttendeeException extends RuntimeException {
  private static final long serialVersionUID = 1;

  public InvalidAttendeeException() {
    super();
  }

  public InvalidAttendeeException(String message) {
    super(message);
  }

  public InvalidAttendeeException(String message, Throwable err) {
    super(message, err);
  }
}
