package tli.ast.varstate;

import java.util.Optional;

import tli.ast.val.Val;

public final class Defined implements VarState {
  private final Val _val;

  public static Defined of(Val val) {
    return new Defined(val);
  }

  public Defined(Val val) {
    _val = val;
  }

  @Override
  public Optional<Val> val() {
    return Optional.of(_val);
  }

  @Override
  public String toString() {
    return _val.toString();
  }
}
