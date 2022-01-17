package ro.arc.packageManager.core.repository;

import ro.arc.packageManager.core.domain.PackageMaintainer;

import java.util.List;

public interface PackageMaintainerRepository extends Repository<PackageMaintainer, Long> {

    List<PackageMaintainer> findAllByPackageID(Long id);
    List<PackageMaintainer> findAllByMaintainerID(Long id);

    boolean existsAllByMaintainerIDAndPackageID(Long maintainerID, Long packageID);
}
