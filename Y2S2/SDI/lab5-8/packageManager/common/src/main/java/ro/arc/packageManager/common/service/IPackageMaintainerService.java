package ro.arc.packageManager.common.service;

import ro.arc.packageManager.common.domain.PackageMaintainer;
import ro.arc.packageManager.common.domain.exceptions.AppException;
import ro.arc.packageManager.common.domain.validators.ValidatorException;

import java.util.concurrent.Future;
import java.util.stream.Stream;

public interface IPackageMaintainerService {

    Future<Void> addPackageMaintainer(Long maintainerID, Long packageID) throws ValidatorException, AppException;

    Future<Void> deletePackageMaintainer(Long id);

    Future<Void> updatePackageMaintainer(Long id, Long maintainerID, Long packageID) throws ValidatorException;

    Future<Stream<PackageMaintainer>> getFilteredPackageMaintainers(String field, String searchTerm);

    Future<Stream<PackageMaintainer>> getAllPackageMaintainers();
}
