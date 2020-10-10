package com.aionescu.shapes;

public class Square implements Shape {
  private final double _sideLength;

  public double getSideLength() {
    return _sideLength;
  }

  public Square(double sideLength) {
    _sideLength = sideLength;
  }

  public String toString() {
    return "Square { sideLength = " + _sideLength + ", perimeter = " + perimeter() + " }";
  }

  public double perimeter() {
    return _sideLength * 4;
  }
}
