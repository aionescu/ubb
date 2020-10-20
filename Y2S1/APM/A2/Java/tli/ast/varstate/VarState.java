package tli.ast.varstate;

import java.util.Optional;

import tli.ast.val.Val;

public interface VarState {
  Optional<Val> val();
}
