package ro.arc.packageManager.server.service;


import ro.arc.packageManager.common.domain.PackageVersion;
import ro.arc.packageManager.common.domain.Package;
import ro.arc.packageManager.common.domain.exceptions.AppException;
import ro.arc.packageManager.common.domain.exceptions.Contract;
import ro.arc.packageManager.common.domain.validators.ValidatorException;
import ro.arc.packageManager.common.service.PackageVersionService;
import ro.arc.packageManager.server.repository.Repository;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

public class PackageVersionServiceImplementation implements PackageVersionService {
    private Repository<Long, Package> packageRepo;
    private Repository<Long, PackageVersion> packageVersionRepo;

    public PackageVersionServiceImplementation(
            Repository<Long, Package> packageRepo,
            Repository<Long, PackageVersion> packageVersionRepo)
    {
        this.packageRepo = packageRepo;
        this.packageVersionRepo = packageVersionRepo;
    }

    @Override
    public void addPackageVersion(PackageVersion packageVersion) throws ValidatorException, AppException {
        packageRepo.findOne(packageVersion.getPackageID()).orElseThrow(() -> new AppException("Inexistent package."));

        Contract.ensure(!existsPackageVersion(packageVersion.getPackageID()), "That package already has a known version.");

        this.packageVersionRepo.save(packageVersion);
    }

    private boolean existsPackageVersion(Long packageID) {
        Contract.notNull(packageID, "packageID");

        var all = this.packageVersionRepo.findAll();

        return
                StreamSupport.stream(all.spliterator(), false)
                        .anyMatch(pv -> pv.getPackageID().equals(packageID));
    }

    @Override
    public void deletePackageVersion(Long id) {
        this.packageVersionRepo.findOne(id).orElseThrow(() -> new AppException("No PackageVersion exists with this ID."));
        this.packageVersionRepo.delete(id);
    }

    @Override
    public void updatePackageVersion(PackageVersion packageVersion) throws ValidatorException {
        this.packageVersionRepo.findOne(packageVersion.getPackageID()).orElseThrow(() -> new AppException("No PackageVersion exists with this ID."));

        this.packageVersionRepo.update(packageVersion);
    }

    @Override
    public List<PackageVersion> getFilteredPackageVersions(String field, String searchTerm) {
        var entities = this.packageVersionRepo.findAll();
        var packageVersions = StreamSupport.stream(entities.spliterator(), false);

        switch (field) {
            case "packageID":
                return packageVersions.filter(p -> String.valueOf(p.getPackageID()).contains(searchTerm)).collect(Collectors.toList());
            case "versionNumber":
                return packageVersions.filter(p -> String.valueOf(p.getVersionNumber()).contains(searchTerm)).collect(Collectors.toList());
            default:
                throw new AppException("This type does not exist !");
        }
    }

    @Override
    public List<PackageVersion> getAllPackageVersions() {
        var packageVersions = this.packageVersionRepo.findAll();
        return StreamSupport.stream(packageVersions.spliterator(), false).collect(Collectors.toList());
    }
}
