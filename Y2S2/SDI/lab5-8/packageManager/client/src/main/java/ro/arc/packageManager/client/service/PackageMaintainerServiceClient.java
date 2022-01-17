package ro.arc.packageManager.client.service;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.stream.Stream;

import ro.arc.packageManager.common.domain.PackageMaintainer;

public final class PackageMaintainerServiceClient extends ServiceClient {
  public PackageMaintainerServiceClient(ExecutorService executorService) {
    super(executorService);
  }

  public Future<Void> addPackageMaintainer(Long maintainerID, Long packageID) {
    return pingPong("add package-maintainer", maintainerID, packageID);
  }

  public Future<Void> deletePackageMaintainer(Long id) {
    return pingPong("delete package-maintainer", id);
  }

  public Future<Void> updatePackageMaintainer(Long id, Long maintainerID, Long packageID) {
    return pingPong("update package-maintainer", id, maintainerID, packageID);
  }

  public Future<Stream<PackageMaintainer>> showPackageMaintainers() {
    return pingPongEntities(PackageMaintainer.class, "show package-maintainers");
  }

  public Future<Stream<PackageMaintainer>> showFilteredPackageMaintainers(String field, String searchTerm) {
    return pingPongEntities(PackageMaintainer.class, "show package-maintainers", field, searchTerm);
  }
}
