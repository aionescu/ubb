package asg01;

public final class IncorrectShapeExtension extends RuntimeException {
  private static final long serialVersionUID = 0;

  public IncorrectShapeExtension(String message, Throwable err) {
    super(message, err);
  }

  public IncorrectShapeExtension(String message) {
    super(message);
  }
}
