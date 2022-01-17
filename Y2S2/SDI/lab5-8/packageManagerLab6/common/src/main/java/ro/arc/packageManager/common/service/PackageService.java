package ro.arc.packageManager.common.service;

import ro.arc.packageManager.common.domain.validators.ValidatorException;
import ro.arc.packageManager.common.domain.Package;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Future;
import java.util.stream.Stream;

public interface PackageService {

    void addPackage(Package pkg) throws ValidatorException;

    void deletePackage(Long ID);

    void updatePackage(Package pkg) throws ValidatorException;

    List<Package> getFilteredPackages(String type, String input);

    List<Package> getAllPackages();
}
