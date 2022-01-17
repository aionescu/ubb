package ro.arc.packageManager.config;

import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

@Configuration
@ComponentScan({"ro.arc.packageManager.repository", "ro.arc.packageManager.service", "ro.arc.packageManager.ui", "ro.arc.packageManager.domain.validators"})
public class PackageManagerConfig {
}
