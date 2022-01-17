import ro.arc.packageManager.common.domain.*;
import ro.arc.packageManager.common.domain.Package;
import ro.arc.packageManager.common.domain.validators.*;
import ro.arc.packageManager.common.service.IMaintainerService;
import ro.arc.packageManager.common.service.IPackageMaintainerService;
import ro.arc.packageManager.common.service.IPackageService;
import ro.arc.packageManager.common.service.IPackageVersionService;
import ro.arc.packageManager.server.repository.DBRepository;
import ro.arc.packageManager.server.repository.Repository;
import ro.arc.packageManager.server.server.TCPServer;
import ro.arc.packageManager.server.service.MaintainerService;
import ro.arc.packageManager.server.service.PackageMaintainerService;
import ro.arc.packageManager.server.service.PackageService;
import ro.arc.packageManager.server.service.PackageVersionService;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class ServerApp {
    public static void main(String[] args) {
        ExecutorService executorService = Executors.newFixedThreadPool(
                Runtime.getRuntime().availableProcessors()
        );
        String url = "jdbc:postgresql://localhost:5432/packageMaintainerJDBC";
        String user = "postgres";
        String password = "password";

        var maintainerHelper = new MaintainerHelper();
        var packageHelper = new PackageHelper();
        var packageMaintainerHelper = new PackageMaintainerHelper();
        var packageVersionHelper = new PackageVersionHelper();

        var maintainerValidator = new MaintainerValidator();
        var packageValidator = new PackageValidator();
        var packageMaintainerValidator = new PackageMaintainerValidator();
        var packageVersionValidator = new PackageVersionValidator();

        Repository<Long, Maintainer> maintainerRepo = null;
        Repository<Long, Package> packageRepo = null;
        Repository<Long, PackageMaintainer> packageMaintainerRepo = null;
        Repository<Long, PackageVersion> packageVersionRepo = null;

        maintainerRepo = new DBRepository<Maintainer>(maintainerValidator, maintainerHelper, url, user, password);
        packageRepo = new DBRepository<Package>(packageValidator, packageHelper, url, user, password);
        packageMaintainerRepo = new DBRepository<PackageMaintainer>(packageMaintainerValidator, packageMaintainerHelper, url, user, password);
        packageVersionRepo = new DBRepository<PackageVersion>(packageVersionValidator,packageVersionHelper,url,user,password);

        IMaintainerService maintainerService = new MaintainerService(maintainerRepo, executorService);
        IPackageService packageService = new PackageService(packageRepo, executorService);
        IPackageMaintainerService packageMaintainerService = new PackageMaintainerService(maintainerRepo, packageRepo, packageMaintainerRepo, executorService);
        IPackageVersionService packageVersionService = new PackageVersionService(packageRepo, packageVersionRepo, executorService);

        TCPServer tcpServer = new TCPServer(executorService, 1234, maintainerService, packageService, packageMaintainerService,packageVersionService);
        tcpServer.startServer();
    }
}
