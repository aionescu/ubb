package ro.arc.packageManager.ui;

import java.io.IOException;

@FunctionalInterface
public interface Command {
  void run() throws IOException;
}
