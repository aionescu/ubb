package tli.view;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import tli.exn.eval.EvalException;
import tli.exn.typeck.TypeCheckerException;
import tli.ast.prog.ProgState;
import tli.controller.*;
import tli.parser.TLParser;
import utils.collections.list.List;
import utils.uparsec.exn.UParsecException;

public final class CLIView implements View {
  private final Controller _controller;

  public CLIView(Controller controller) {
    _controller = controller;
  }

  private void _handleCommand(String line) throws IOException {
    var parts = line.split("\\s+", 2);

    switch (parts[0]) {
      case "load":
        var code = Files.readString(Path.of(parts[1]));
        var ast = TLParser.parse(code);
        _controller.setState(ProgState.empty.withToDo(List.singleton(ast)));
        break;

      case "typeck":
        _controller.typeCheck();
        System.out.println("Type check succeeded.");
        break;

      case "show-state":
        System.out.println(_controller.state());
        break;

      case "run":
        var state = _controller.state();

        _controller.typeCheck();
        _controller.allSteps();
        System.out.println(_controller.state().output());

        _controller.setState(state);
        break;

      case "all-steps":
        _controller.allSteps().map(Object::toString).iter(System.out::println);
        break;

      case "one-step":
        _controller.oneStep();
        System.out.println(_controller.state());
        break;

      case "parse":
        var ast_ = TLParser.parse(parts[1]);
        _controller.setState(ProgState.empty.withToDo(List.singleton(ast_)));
        break;

      case "exit":
        System.exit(0);
        break;

      default:
        System.out.println("Unrecognized command");
        break;
    }
  }

  @Override
  public void run() {
    var console = System.console();

    while (true) {
      try {
        System.out.print("\ntli> ");
        var line = console.readLine();

        _handleCommand(line);
      } catch (IOException e) {
        System.out.println("IO Error: " + e.getMessage());
      } catch (UParsecException e) {
        System.out.println("Parser error: " + e.getMessage());
      } catch (TypeCheckerException e) {
        System.out.println("Type error: " + e.getMessage());
      } catch (EvalException e) {
        System.out.println("Evaluation error: " + e.getMessage());
      } catch (Throwable e) {
        System.out.println("Something unexpected occurred:");
        e.printStackTrace();
      }
    }
  }
}
