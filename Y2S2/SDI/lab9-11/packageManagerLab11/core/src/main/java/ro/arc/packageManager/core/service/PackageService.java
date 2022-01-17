package ro.arc.packageManager.core.service;

import ro.arc.packageManager.core.domain.Package;

import java.util.List;

public interface PackageService {
    Package addPackage(Package pkg);

    Package addPackageAnyCase(Package pkg);

    Package updatePackage(Package pkg);

    void deletePackage(Long ID);

    List<Package> getFilteredPackages(String type, String input);

    List<Package> getAllPackages();

    Package getOnePackage(Long ID);

    Package getPackageByName(String name);
}
