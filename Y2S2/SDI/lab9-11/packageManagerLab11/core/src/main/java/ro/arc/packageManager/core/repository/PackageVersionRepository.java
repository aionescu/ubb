package ro.arc.packageManager.core.repository;

import ro.arc.packageManager.core.domain.Package;
import ro.arc.packageManager.core.domain.PackageVersion;

import java.util.List;

public interface PackageVersionRepository extends Repository<PackageVersion, Long> {

    List<PackageVersion> findAllByAPackage(Package aPackage);
    List<PackageVersion> findAllByVersionNumberContains(String input);
    boolean existsAllByVersionNumberAndIdIsNot(String versionNumber, Long id);
    boolean existsAllByVersionNumber(String versionNumber);

}