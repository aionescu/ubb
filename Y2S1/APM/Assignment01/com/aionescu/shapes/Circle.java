package com.aionescu.shapes;

import java.lang.Math;

public class Circle implements Shape {
  private final double _radius;

  public double getRadius() {
    return _radius;
  }

  public Circle(double radius) {
    _radius = radius;
  }

  public String toString() {
    return "Circle { radius = " + _radius + ", perimeter = " + perimeter() + " }";
  }

  public double perimeter() {
    return _radius * 2 * Math.PI;
  }
}
