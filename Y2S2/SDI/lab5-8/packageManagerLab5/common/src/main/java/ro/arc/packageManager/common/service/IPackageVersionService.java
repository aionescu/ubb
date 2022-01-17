package ro.arc.packageManager.common.service;

import ro.arc.packageManager.common.domain.PackageVersion;
import ro.arc.packageManager.common.domain.validators.ValidatorException;

import java.util.concurrent.Future;
import java.util.stream.Stream;

public interface IPackageVersionService {
    Future<Void> addPackageVersion(Long packageID, String versionNumber) throws ValidatorException;

    Future<Void> deletePackageVersion(Long ID);

    Future<Void> updatePackageVersion(Long ID, Long packageID, String versionNumber) throws ValidatorException;

    Future<Stream<PackageVersion>> getFilteredPackageVersions(String type, String input);

    Future<Stream<PackageVersion>> getAllPackageVersions();

}
