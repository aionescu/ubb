package ro.arc.packageManager.client.service;

import org.springframework.beans.factory.annotation.Autowired;
import ro.arc.packageManager.common.domain.PackageMaintainer;
import ro.arc.packageManager.common.service.PackageMaintainerService;

import java.util.List;

public class PackageMaintainerServiceClientImplementation implements PackageMaintainerService {
    @Autowired
    private PackageMaintainerService packageMaintainerService;

    @Override
    public void addPackageMaintainer(PackageMaintainer packageVersion) {
        this.packageMaintainerService.addPackageMaintainer(packageVersion);
    }

    @Override
    public void updatePackageMaintainer(PackageMaintainer packageVersion) {
        this.packageMaintainerService.updatePackageMaintainer(packageVersion);
    }

    @Override
    public void deletePackageMaintainer(Long ID) {
        this.packageMaintainerService.deletePackageMaintainer(ID);
    }

    @Override
    public List<PackageMaintainer> getFilteredPackageMaintainers(String type, String input) {
        return this.packageMaintainerService.getFilteredPackageMaintainers(type, input);
    }

    @Override
    public List<PackageMaintainer> getAllPackageMaintainers() {
        return this.packageMaintainerService.getAllPackageMaintainers();
    }
}
