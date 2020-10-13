package asg01;

public final class Square implements Shape {
  private final double _sideLength;

  public double sideLength() {
    return _sideLength;
  }

  public Square(double sideLength) {
    _sideLength = sideLength;
  }

  @Override
  public String toString() {
    return "Square { sideLength = " + _sideLength + ", perimeter = " + perimeter() + " }";
  }

  @Override
  public double perimeter() {
    return _sideLength * 4;
  }
}
