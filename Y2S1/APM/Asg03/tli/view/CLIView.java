package tli.view;

import tli.controller.*;

public final class CLIView implements View {
  private final Controller _controller;

  public CLIView(Controller controller) {
    _controller = controller;
  }

  @Override
  public void run() {
    _controller.typeCheck();
    _controller.allSteps().map(Object::toString).iter(System.out::println);

    System.out.println("\nOutput:");
    System.out.println(_controller.state().output());
  }
}
