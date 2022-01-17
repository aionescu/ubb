package ro.arc.packageManager.common.service;

import ro.arc.packageManager.common.domain.PackageVersion;
import ro.arc.packageManager.common.domain.validators.ValidatorException;

import java.util.List;

public interface PackageVersionService {

    void addPackageVersion(PackageVersion packageVersion) throws ValidatorException;

    void deletePackageVersion(Long ID);

    void updatePackageVersion(PackageVersion packageVersion) throws ValidatorException;

    List<PackageVersion> getFilteredPackageVersions(String type, String input);

    List<PackageVersion> getAllPackageVersions();

}
