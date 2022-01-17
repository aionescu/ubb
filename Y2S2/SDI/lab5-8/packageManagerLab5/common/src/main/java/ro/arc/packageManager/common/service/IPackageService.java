package ro.arc.packageManager.common.service;

import ro.arc.packageManager.common.domain.validators.ValidatorException;
import ro.arc.packageManager.common.domain.Package;

import java.util.concurrent.Future;
import java.util.stream.Stream;

public interface IPackageService {

    Future<Void> addPackage(String name, String description, String sourceRepo, String license) throws ValidatorException;

    Future<Void> deletePackage(Long ID);

    Future<Void> updatePackage(Long ID, String name, String description, String sourceRepo, String license) throws ValidatorException;

    Future<Stream<Package>> getFilteredPackages(String type, String input);

    Future<Stream<Package>> getAllPackages();
}
