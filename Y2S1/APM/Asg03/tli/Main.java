package tli;

import java.util.Arrays;

import utils.collections.list.List;
import tli.ast.Ident;
import tli.ast.type.Type;
import tli.ast.val.*;
import tli.ast.expr.*;
import tli.ast.stmt.*;
import tli.ast.prog.ProgState;
import tli.repo.SingleStateRepository;
import tli.controller.Controller;
import tli.view.CLIView;

public final class Main {
  public static void main(String[] args) {
    Stmt[] stmts = {
      Decl.of(Type.INT, Ident.of("a")),
      Assign.of(Ident.of("a"), Lit.of(Int.of(0))),
      While.of(
        Compare.of(Var.of(Ident.of("a")), Compare.Op.LT, Lit.of(Int.of(10))),
        Comp.of(
          Print.of(Var.of(Ident.of("a"))),
          Assign.of(
            Ident.of("a"),
            Arith.of(Var.of(Ident.of("a")), Arith.Op.ADD, Lit.of(Int.of(1))))))
    };

    var state = ProgState.empty.withToDo(List.ofStream(Arrays.stream(stmts)));

    var repo = new SingleStateRepository(state);
    var controller = new Controller(repo);
    var view = new CLIView(controller);

    view.run();
  }
}
