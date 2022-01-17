package ro.arc.packageManager.client;

import java.util.concurrent.Executors;

import ro.arc.packageManager.client.service.MaintainerServiceClient;
import ro.arc.packageManager.client.service.PackageMaintainerServiceClient;
import ro.arc.packageManager.client.service.PackageServiceClient;
import ro.arc.packageManager.client.service.PackageVersionServiceClient;
import ro.arc.packageManager.client.ui.TUI;

public class Main {
  public static void main(String[] args) {
    var executorService = Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors());
    var maintainerService = new MaintainerServiceClient(executorService);
    var packageService = new PackageServiceClient(executorService);
    var packageMaintainerService = new PackageMaintainerServiceClient(executorService);
    var packageVersionService = new PackageVersionServiceClient(executorService);

    var ui = new TUI(maintainerService, packageService, packageMaintainerService,packageVersionService);

    ui.run();
    executorService.shutdown();
  }
}
