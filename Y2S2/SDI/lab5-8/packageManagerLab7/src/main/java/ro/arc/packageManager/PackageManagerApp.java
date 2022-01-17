package ro.arc.packageManager;

import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import ro.arc.packageManager.ui.Console;

public class PackageManagerApp {
    public static void main(String[] args) {
        AnnotationConfigApplicationContext context =
                new AnnotationConfigApplicationContext("ro.arc.packageManager.config");

        Console console = context.getBean(Console.class);
        console.run();
    }
}
