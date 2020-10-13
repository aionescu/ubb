package asg02.exn;

public class DuplicateAttendeeException extends RuntimeException {
  private static final long serialVersionUID = 1;

  public DuplicateAttendeeException() {
    super();
  }

  public DuplicateAttendeeException(String message) {
    super(message);
  }

  public DuplicateAttendeeException(String message, Throwable err) {
    super(message, err);
  }
}
