package asg01;

public final class Main {
  public static void main(String[] args) {
    var shapeCollection = new ShapeCollection(
      new Triangle(1, 2, 3),
      new Circle(2.25),
      new Square(4));

    shapeCollection
      .shapes()
      .stream()
      .forEach(System.out::println);
  }
}
