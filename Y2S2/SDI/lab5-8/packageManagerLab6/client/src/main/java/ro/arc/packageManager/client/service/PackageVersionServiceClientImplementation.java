package ro.arc.packageManager.client.service;

import org.springframework.beans.factory.annotation.Autowired;
import ro.arc.packageManager.common.domain.Package;
import ro.arc.packageManager.common.domain.PackageVersion;
import ro.arc.packageManager.common.service.PackageService;
import ro.arc.packageManager.common.service.PackageVersionService;

import java.util.List;

public class PackageVersionServiceClientImplementation implements PackageVersionService {
    @Autowired
    private PackageVersionService packageVersionService;

    @Override
    public void addPackageVersion(PackageVersion packageVersion) {
        this.packageVersionService.addPackageVersion(packageVersion);
    }

    @Override
    public void updatePackageVersion(PackageVersion packageVersion) {
        this.packageVersionService.updatePackageVersion(packageVersion);
    }

    @Override
    public void deletePackageVersion(Long ID) {
        this.packageVersionService.deletePackageVersion(ID);
    }

    @Override
    public List<PackageVersion> getFilteredPackageVersions(String type, String input) {
        return this.packageVersionService.getFilteredPackageVersions(type, input);
    }

    @Override
    public List<PackageVersion> getAllPackageVersions() {
        return this.packageVersionService.getAllPackageVersions();
    }
}
