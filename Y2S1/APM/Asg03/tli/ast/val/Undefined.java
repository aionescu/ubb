package tli.ast.val;

public final class Undefined implements Val {
  public static final Undefined value = new Undefined();

  private Undefined() { }

  @Override
  public String toString() {
    return "Undefined";
  }
}
