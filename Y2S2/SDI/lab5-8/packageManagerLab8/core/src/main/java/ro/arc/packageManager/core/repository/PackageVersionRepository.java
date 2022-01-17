package ro.arc.packageManager.core.repository;

import ro.arc.packageManager.core.domain.PackageVersion;

import java.util.List;

public interface PackageVersionRepository extends Repository<PackageVersion, Long> {

    List<PackageVersion> findAllByPackageID(Long id);
    List<PackageVersion> findAllByVersionNumberContains(String input);

    boolean existsAllByPackageID(Long packageID);
}