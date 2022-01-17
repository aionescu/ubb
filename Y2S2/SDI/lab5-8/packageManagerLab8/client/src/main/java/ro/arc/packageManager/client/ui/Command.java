package ro.arc.packageManager.client.ui;

import java.io.IOException;

@FunctionalInterface
public interface Command {
  void run() throws IOException;
}
