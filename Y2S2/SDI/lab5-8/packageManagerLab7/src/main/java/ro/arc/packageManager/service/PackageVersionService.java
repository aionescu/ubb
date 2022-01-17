package ro.arc.packageManager.service;

import ro.arc.packageManager.domain.Package;
import ro.arc.packageManager.domain.PackageVersion;

import java.util.List;

public interface PackageVersionService {
    void addPackageVersion(PackageVersion packageVersion);

    void updatePackageVersion(PackageVersion packageVersion);

    void deletePackageVersion(Long ID);

    List<PackageVersion> getFilteredPackageVersions(String type, String input);

    List<PackageVersion> getAllPackageVersions();
}