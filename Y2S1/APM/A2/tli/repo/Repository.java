package tli.repo;

import tli.ast.prog.ProgState;

public interface Repository {
  ProgState state();
  void setState(ProgState state);
  void typeCheck();
  void oneStep();
  boolean done();
}
