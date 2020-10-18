package tli.ast.val;

public final class Bool implements Val {
  public final boolean val;

  public Bool(boolean val) {
    this.val = val;
  }

  @Override
  public String toString() {
    return val ? "True" : "False";
  }
}
