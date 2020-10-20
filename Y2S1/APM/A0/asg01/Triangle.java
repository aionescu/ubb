package asg01;

public class Triangle implements Shape {
  private final double _sideA, _sideB, _sideC;

  public double sideA() {
    return _sideA;
  }

  public double sideB() {
    return _sideB;
  }

  public double sideC() {
    return _sideC;
  }

  public Triangle(double sideA, double sideB, double sideC) {
    _sideA = sideA;
    _sideB = sideB;
    _sideC = sideC;
  }

  @Override
  public String toString() {
    return "Triangle { sides = (" + _sideA + ", " + _sideB + ", " + _sideC + "), perimeter = " + perimeter() + " }";
  }

  @Override
  public double perimeter() {
    return _sideA + _sideB + _sideC;
  }
}
