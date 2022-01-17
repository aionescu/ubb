package ro.arc.packageManager.server.config;

import org.postgresql.shaded.com.ongres.scram.common.bouncycastle.pbkdf2.Pack;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.remoting.rmi.RmiServiceExporter;
import ro.arc.packageManager.common.domain.*;
import ro.arc.packageManager.common.domain.Package;
import ro.arc.packageManager.common.domain.validators.MaintainerValidator;
import ro.arc.packageManager.common.domain.validators.PackageMaintainerValidator;
import ro.arc.packageManager.common.domain.validators.PackageValidator;
import ro.arc.packageManager.common.domain.validators.PackageVersionValidator;
import ro.arc.packageManager.common.service.MaintainerService;
import ro.arc.packageManager.common.service.PackageMaintainerService;
import ro.arc.packageManager.common.service.PackageService;
import ro.arc.packageManager.common.service.PackageVersionService;
import ro.arc.packageManager.server.repository.DBRepository;
import ro.arc.packageManager.server.repository.Repository;
import ro.arc.packageManager.server.service.MaintainerServiceImplementation;
import ro.arc.packageManager.server.service.PackageMaintainerServiceImplementation;
import ro.arc.packageManager.server.service.PackageServiceImplementation;
import ro.arc.packageManager.server.service.PackageVersionServiceImplementation;

@Configuration
public class ServerAppConfig {

    @Bean
    RmiServiceExporter rmiServiceExporterMaintainer() {
        RmiServiceExporter rmiServiceExporter = new RmiServiceExporter();
        rmiServiceExporter.setServiceInterface(MaintainerService.class);
        rmiServiceExporter.setService(maintainerService());
        rmiServiceExporter.setServiceName("MaintainerService");
        return rmiServiceExporter;
    }

    @Bean
    MaintainerService maintainerService() {
        Repository<Long, Maintainer> maintainerRepo = maintainerRepo();
        return new MaintainerServiceImplementation(maintainerRepo);
    }

    @Bean
    Repository<Long, Maintainer> maintainerRepo() {
        MaintainerValidator validator = new MaintainerValidator();
        MaintainerHelper helper = new MaintainerHelper();
        return new DBRepository<>(validator, helper);
    }

    @Bean
    RmiServiceExporter rmiServiceExporterPackage() {
        RmiServiceExporter rmiServiceExporter = new RmiServiceExporter();
        rmiServiceExporter.setServiceInterface(PackageService.class);
        rmiServiceExporter.setService(packageService());
        rmiServiceExporter.setServiceName("PackageService");
        return rmiServiceExporter;
    }

    @Bean
    PackageService packageService(){
        Repository <Long, Package> packageRepo = packageRepo();
        return new PackageServiceImplementation(packageRepo);
    }

    @Bean
    Repository<Long, Package> packageRepo(){
        PackageValidator validator = new PackageValidator();
        PackageHelper helper = new PackageHelper();
        return new DBRepository<>(validator,helper);
    }

    @Bean
    RmiServiceExporter rmiServiceExporterPackageVersion(){
        RmiServiceExporter rmiServiceExporter = new RmiServiceExporter();
        rmiServiceExporter.setServiceInterface(PackageVersionService.class);
        rmiServiceExporter.setService(packageVersionService());
        rmiServiceExporter.setServiceName("PackageVersionService");
        return rmiServiceExporter;
    }

    @Bean
    PackageVersionService packageVersionService(){
        Repository <Long, PackageVersion> packageVersionRepo = packageVersionRepo();
        Repository <Long, Package> packageRepo = packageRepo();
        return new PackageVersionServiceImplementation(packageRepo,packageVersionRepo);
    }

    @Bean
    Repository<Long, PackageVersion> packageVersionRepo(){
        PackageVersionValidator validator = new PackageVersionValidator();
        PackageVersionHelper helper = new PackageVersionHelper();
        return new DBRepository<>(validator,helper);
    }

    @Bean
    RmiServiceExporter rmiServiceExporterPackageMaintainer(){
        RmiServiceExporter rmiServiceExporter = new RmiServiceExporter();
        rmiServiceExporter.setServiceInterface(PackageMaintainerService.class);
        rmiServiceExporter.setService(packageMaintainerService());
        rmiServiceExporter.setServiceName("PackageMaintainerService");
        return rmiServiceExporter;
    }

    @Bean
    PackageMaintainerService packageMaintainerService(){
        Repository <Long, PackageMaintainer> packageMaintainerRepo = packageMaintainerRepo();
        Repository <Long, Package> packageRepo = packageRepo();
        var maintainerRepo = maintainerRepo();
        return new PackageMaintainerServiceImplementation(maintainerRepo, packageRepo, packageMaintainerRepo);
    }

    @Bean
    Repository<Long, PackageMaintainer> packageMaintainerRepo(){
        PackageMaintainerValidator validator = new PackageMaintainerValidator();
        PackageMaintainerHelper helper = new PackageMaintainerHelper();
        return new DBRepository<>(validator,helper);
    }
}
