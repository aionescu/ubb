package tli;

import java.util.Arrays;

import tli.ast.Ident;
import tli.ast.expr.Arith;
import tli.ast.expr.Lit;
import tli.ast.expr.Var;
import tli.ast.expr.Arith.Op;
import tli.ast.prog.ProgState;
import tli.ast.stmt.Assign;
import tli.ast.stmt.Decl;
import tli.ast.stmt.Print;
import tli.ast.stmt.Stmt;
import tli.ast.type.Type;
import tli.ast.val.Int;
import tli.controller.Controller;
import tli.repo.SingleStateRepository;
import tli.view.CLIView;
import utils.collections.list.List;

public final class Main {
  public static void main(String[] args) {
    Stmt[] stmts = {
      new Decl(Type.INT, new Ident("a")),
      new Assign(new Ident("a"), new Lit(new Int(2))),
      new Assign(new Ident("a"), new Arith(new Var(new Ident("a")), Op.ADD, new Var(new Ident("a")))),
      new Print(new Var(new Ident("a")))
    };

    var state = ProgState.empty.withToDo(List.ofStream(Arrays.stream(stmts)));

    var repo = new SingleStateRepository(state);
    var controller = new Controller(repo);
    var view = new CLIView(controller);

    view.run();
  }
}
