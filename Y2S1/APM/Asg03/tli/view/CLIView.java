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
    _controller.allSteps();
    System.out.println(_controller.state().output());
  }
}
