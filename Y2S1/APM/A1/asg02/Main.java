// 9. La o conferinta participa profesori, studenti si
// specialisti din industrie. Sa se afiseze toti participantii
// care au prezentat o lucrare.

package asg02;

import asg02.controller.Controller;
import asg02.repo.InMemoryRepository;
import asg02.view.CLIView;

public final class Main {
  public static void main(String[] args) {
    var repo = new InMemoryRepository(100);
    var controller = new Controller(repo);
    var view = new CLIView(controller);

    view.run();
  }
}
