package edu.asg02.view;

import edu.asg02.controller.Controller;

public class CLIView implements View {
  private final Controller _controller;

  public CLIView(Controller controller) {
    if (controller == null)
      throw new IllegalArgumentException();

    _controller = controller;
  }

  public void run() {

  }
}
