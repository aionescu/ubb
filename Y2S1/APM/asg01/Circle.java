package asg01;

import java.lang.Math;

public final class Circle implements Shape {
  private final double _radius;

  public double radius() {
    return _radius;
  }

  public Circle(double radius) {
    _radius = radius;
  }

  @Override
  public String toString() {
    return "Circle { radius = " + _radius + ", perimeter = " + perimeter() + " }";
  }

  @Override
  public double perimeter() {
    return _radius * 2 * Math.PI;
  }
}
