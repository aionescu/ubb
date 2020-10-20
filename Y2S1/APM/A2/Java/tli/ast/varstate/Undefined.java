package tli.ast.varstate;

import java.util.Optional;

import tli.ast.val.Val;

public final class Undefined implements VarState {
  public static Undefined value = new Undefined();

  private Undefined() { }

  @Override
  public Optional<Val> val() {
    return Optional.empty();
  }

  @Override
  public String toString() {
    return "!";
  }
}
