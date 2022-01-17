package ro.arc.packageManager.service;

import ro.arc.packageManager.domain.Package;

import java.util.List;

public interface PackageService {
    void addPackage(Package pkg);

    void updatePackage(Package pkg);

    void deletePackage(Long ID);

    List<Package> getFilteredPackages(String type, String input);

    List<Package> getAllPackages();
}
