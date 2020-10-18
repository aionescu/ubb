package tli.ast;

public final class Ident {
  public final String name;

  public Ident(String name) {
    this.name = name;
  }

  @Override
  public int hashCode() {
    return name.hashCode();
  }

  @Override
  public boolean equals(Object rhs) {
    if (rhs instanceof Ident) {
      return name.equals(((Ident)rhs).name);
    } else
      throw new IllegalArgumentException("Can't compare Ident to non-Ident.");
  }

  @Override
  public String toString() {
    return name;
  }
}
