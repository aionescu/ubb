package asg01;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public final class ShapeCollection {
  private final ArrayList<Shape> _shapes;

  public ShapeCollection(Shape... shapes) {
    _shapes = new ArrayList<>(Arrays.asList(shapes));
  }

  public void addShape(Shape shape) throws IncorrectShapeExtension {
    if (shape.perimeter() <= 0)
      throw new IncorrectShapeExtension("Shapes must have a positive perimeter.");

    _shapes.add(shape);
  }

  public List<Shape> shapes() {
    return List.copyOf(_shapes);
  }
}
