package ro.arc.packageManager.common.service;

import ro.arc.packageManager.common.domain.PackageMaintainer;
import ro.arc.packageManager.common.domain.exceptions.AppException;
import ro.arc.packageManager.common.domain.validators.ValidatorException;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Future;
import java.util.stream.Stream;

public interface PackageMaintainerService {

    void addPackageMaintainer(PackageMaintainer packageMaintainer) throws ValidatorException, AppException;

    void deletePackageMaintainer(Long id);

    void updatePackageMaintainer(PackageMaintainer packageMaintainer) throws ValidatorException;

    List<PackageMaintainer> getFilteredPackageMaintainers(String field, String searchTerm);

    List<PackageMaintainer> getAllPackageMaintainers();
}
