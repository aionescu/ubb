package ro.arc.packageManager.client.service;

import ro.arc.packageManager.common.domain.Package;
import ro.arc.packageManager.common.domain.PackageVersion;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.stream.Stream;

public class PackageVersionServiceClient extends ServiceClient {
    public PackageVersionServiceClient(ExecutorService executorService) {
        super(executorService);
    }

    public Future<Void> addPackageVersion(Long packageID, String versionNumber) {
        return pingPong("add packageVersion", packageID, versionNumber);
    }

    public Future<Void> deletePackageVersion(Long id) {
        return pingPong("delete packageVersion", id);
    }

    public Future<Void> updatePackageVersion(Long id, Long packageID, String versionNumber) {
        return pingPong("update packageVersion", id, packageID, versionNumber);
    }

    public Future<Stream<PackageVersion>> showPackageVersions() {
        return pingPongEntities(PackageVersion.class, "show packageVersions");
    }

    public Future<Stream<PackageVersion>> showFilteredPackageVersions(String field, String searchTerm) {
        return pingPongEntities(PackageVersion.class, "show packageVersions", field, searchTerm);
    }
}