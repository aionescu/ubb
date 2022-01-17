package ro.arc.packageManager.server;

import org.springframework.context.annotation.AnnotationConfigApplicationContext;

public class ServerApp {
    public static void main(String[] args) {
        System.out.println("server starting...");

        AnnotationConfigApplicationContext context =
                new AnnotationConfigApplicationContext("ro.arc.packageManager.server.config");

    }
}
