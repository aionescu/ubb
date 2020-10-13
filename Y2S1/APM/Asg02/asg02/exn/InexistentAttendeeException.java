package asg02.exn;

public class InexistentAttendeeException extends RuntimeException {
  private static final long serialVersionUID = 1;

  public InexistentAttendeeException() {
    super();
  }

  public InexistentAttendeeException(String message) {
    super(message);
  }

  public InexistentAttendeeException(String message, Throwable err) {
    super(message, err);
  }
}
