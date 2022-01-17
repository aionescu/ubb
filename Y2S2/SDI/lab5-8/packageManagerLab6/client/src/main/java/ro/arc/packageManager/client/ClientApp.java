package ro.arc.packageManager.client;

import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import ro.arc.packageManager.client.ui.ConsoleClient;

public class ClientApp {
    public static void main(String[] args) {
      try (var context = new AnnotationConfigApplicationContext("ro.arc.packageManager.client.config")) {
        ConsoleClient consoleClient = context.getBean(ConsoleClient.class);
        consoleClient.run();
      }
    }
}
