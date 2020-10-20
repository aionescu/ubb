package tli;

import tli.repo.SingleStateRepository;
import tli.controller.Controller;
import tli.view.CLIView;

public final class Main {
  public static void main(String[] args) {
    var repo = new SingleStateRepository();
    var controller = new Controller(repo);
    var view = new CLIView(controller);

    view.run();
  }
}
