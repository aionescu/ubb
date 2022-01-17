package ro.arc.packageManager.client.service;


import org.springframework.beans.factory.annotation.Autowired;
import ro.arc.packageManager.common.domain.Package;
import ro.arc.packageManager.common.service.PackageService;


import java.util.List;

public class PackageServiceClientImplementation implements PackageService {

    @Autowired
    private PackageService packageService;

    @Override
    public void addPackage(Package pkg) {
        this.packageService.addPackage(pkg);
    }

    @Override
    public void updatePackage(Package pkg) {
        this.packageService.updatePackage(pkg);
    }

    @Override
    public void deletePackage(Long ID) {
        this.packageService.deletePackage(ID);
    }

    @Override
    public List<Package> getFilteredPackages(String type, String input) {
        return this.packageService.getFilteredPackages(type, input);
    }

    @Override
    public List<Package> getAllPackages() {
        return this.packageService.getAllPackages();
    }
}
