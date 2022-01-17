package ro.arc.packageManager.domain.exceptions;

public class AppException extends RuntimeException {
  private static final long serialVersionUID = 1;

  public AppException(String message) {
    super(message);
  }

  public AppException(String message, Throwable cause) {
    super(message, cause);
  }

  public AppException(Throwable cause) {
    super(cause);
  }
}
