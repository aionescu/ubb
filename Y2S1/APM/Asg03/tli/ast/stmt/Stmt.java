package tli.ast.stmt;

import utils.collections.map.Map;

import tli.ast.Ident;
import tli.ast.prog.ProgState;
import tli.ast.type.Type;

public interface Stmt {
  Map<Ident, Type> typeCheck(Map<Ident, Type> sym);
  ProgState eval(ProgState prog);
}
