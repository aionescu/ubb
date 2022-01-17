package ro.arc.packageManager.client.service;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.stream.Stream;

import ro.arc.packageManager.common.domain.Package;

public final class PackageServiceClient extends ServiceClient {
  public PackageServiceClient(ExecutorService executorService) {
    super(executorService);
  }

  public Future<Void> addPackage(String name, String description, String sourceRepo, String license) {
    return pingPong("add package", name, description, sourceRepo, license);
  }

  public Future<Void> deletePackage(Long id) {
    return pingPong("delete package", id);
  }

  public Future<Void> updatePackage(Long id, String name, String description, String sourceRepo, String license) {
    return pingPong("update package", id, name, description, sourceRepo, license);
  }

  public Future<Stream<Package>> showPackages() {
    return pingPongEntities(Package.class, "show packages");
  }

  public Future<Stream<Package>> showFilteredPackages(String field, String searchTerm) {
    return pingPongEntities(Package.class, "show packages", field, searchTerm);
  }
}
