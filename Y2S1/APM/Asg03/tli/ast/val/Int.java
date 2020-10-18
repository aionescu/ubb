package tli.ast.val;

public final class Int implements Val {
  public final int val;

  public Int(int val) {
    this.val = val;
  }

  @Override
  public String toString() {
    return String.valueOf(val);
  }
}
