package com.aionescu.shapes;

public class Triangle implements Shape {
  private final double _sideA, _sideB, _sideC;

  public double getSideA() {
    return _sideA;
  }

  public double getSideB() {
    return _sideB;
  }

  public double getSideC() {
    return _sideC;
  }

  public Triangle(double sideA, double sideB, double sideC) {
    _sideA = sideA;
    _sideB = sideB;
    _sideC = sideC;
  }

  public String toString() {
    return "Triangle { sides = (" + _sideA + ", " + _sideB + ", " + _sideC + "), perimeter = " + perimeter() + " }";
  }

  public double perimeter() {
    return _sideA + _sideB + _sideC;
  }
}
