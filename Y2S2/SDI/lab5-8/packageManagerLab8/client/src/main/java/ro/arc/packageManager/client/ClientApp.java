package ro.arc.packageManager.client;

import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.web.client.RestTemplate;
import ro.arc.packageManager.client.ui.Console;

public class ClientApp {

    public static void main(String[] args) {
        AnnotationConfigApplicationContext context =
                new AnnotationConfigApplicationContext("ro.arc.packageManager.client.config");
        Console console = context.getBean(Console.class);
        console.run();
    }
}