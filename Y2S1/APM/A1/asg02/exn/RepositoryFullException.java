package asg02.exn;

public class RepositoryFullException extends RuntimeException {
  private static final long serialVersionUID = 1;

  public RepositoryFullException() {
    super();
  }

  public RepositoryFullException(String message) {
    super(message);
  }

  public RepositoryFullException(String message, Throwable err) {
    super(message, err);
  }
}
