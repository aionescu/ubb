package tli.ast.val;

public final class Bool implements Val {
  public final boolean val;

  public static Bool of(boolean val) {
    return new Bool(val);
  }

  public Bool(boolean val) {
    this.val = val;
  }

  @Override
  public String toString() {
    return val ? "True" : "False";
  }
}
