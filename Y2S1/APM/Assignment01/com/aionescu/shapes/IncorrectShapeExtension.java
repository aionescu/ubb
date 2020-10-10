package com.aionescu.shapes;

import java.lang.Exception;

public class IncorrectShapeExtension extends Exception {
  private static final long serialVersionUID = 0; // ?

  public IncorrectShapeExtension(String message, Throwable err) {
    super(message, err);
  }

  public IncorrectShapeExtension(String message) {
    super(message);
  }
}
