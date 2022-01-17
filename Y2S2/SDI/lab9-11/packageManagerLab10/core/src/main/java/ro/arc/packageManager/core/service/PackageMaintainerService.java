package ro.arc.packageManager.core.service;

import ro.arc.packageManager.core.domain.PackageMaintainer;

import java.util.List;

public interface PackageMaintainerService {
    PackageMaintainer addPackageMaintainer(PackageMaintainer packageMaintainer);

    PackageMaintainer updatePackageMaintainer(PackageMaintainer packageMaintainer);

    void deletePackageMaintainer(Long ID);

    List<PackageMaintainer> getFilteredPackageMaintainers(String type, String input);

    List<PackageMaintainer> getAllPackageMaintainers();
}