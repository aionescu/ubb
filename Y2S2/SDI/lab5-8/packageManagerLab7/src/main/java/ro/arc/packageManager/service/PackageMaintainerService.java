package ro.arc.packageManager.service;

import ro.arc.packageManager.domain.PackageMaintainer;

import java.util.List;

public interface PackageMaintainerService {
    void addPackageMaintainer(PackageMaintainer packageMaintainer);

    void updatePackageMaintainer(PackageMaintainer packageMaintainer);

    void deletePackageMaintainer(Long ID);

    List<PackageMaintainer> getFilteredPackageMaintainers(String type, String input);

    List<PackageMaintainer> getAllPackageMaintainers();
}