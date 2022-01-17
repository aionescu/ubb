package ro.arc.packageManager.server.service;


import ro.arc.packageManager.common.domain.Package;
import ro.arc.packageManager.common.domain.PackageVersion;
import ro.arc.packageManager.common.domain.exceptions.AppException;
import ro.arc.packageManager.common.domain.exceptions.Contract;
import ro.arc.packageManager.common.domain.validators.ValidatorException;
import ro.arc.packageManager.common.service.IPackageVersionService;
import ro.arc.packageManager.server.repository.Repository;

import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import static java.lang.Long.parseLong;

public class PackageVersionService implements IPackageVersionService {

    private Repository<Long, Package> packageRepo;
    private Repository<Long, PackageVersion> packageVersionRepo;
    private ExecutorService executorService;

    public PackageVersionService(Repository<Long, Package> packageRepo, Repository<Long, PackageVersion> packageVersionRepo,ExecutorService executorService) {
        this.packageRepo = packageRepo;
        this.packageVersionRepo = packageVersionRepo;
        this.executorService = executorService;
    }

    @Override
    public Future<Void> addPackageVersion(Long packageID, String versionNumber) throws ValidatorException {
        Callable<Void> callable = () -> {
            Contract.ensure(!existsPackageID(packageID), "A package version already exists for the given packageID.");

            var packageVersion = new PackageVersion(0l, packageID, versionNumber);
            this.packageVersionRepo.save(packageVersion);
            return null;
        };
        return this.executorService.submit(callable);
    }

    private boolean existsPackageID(Long packageID) {
        Contract.notNull(packageID, "packageID");

        var all = packageVersionRepo.findAll();

        return StreamSupport.stream(all.spliterator(), false).anyMatch(m -> m.getPackageID().equals(packageID));
    }

    private boolean samePackageIDDifferentID(Long ID, Long packageID)
    {
        Contract.notNull(packageID, "packageID");
        Contract.notNull(ID, "ID");

        var all = packageVersionRepo.findAll();

        return StreamSupport.stream(all.spliterator(), false).anyMatch(m -> m.getPackageID().equals(packageID) && !m.getID().equals(ID));
    }

    @Override
    public Future<Void> deletePackageVersion(Long ID){
        Callable<Void> callable = () -> {
            this.packageVersionRepo.findOne(ID).orElseThrow(() -> new AppException("Non-existent package version with this ID !"));
            this.packageVersionRepo.delete(ID);
            return null;
        };
        return this.executorService.submit(callable);
    }

    @Override
    public Future<Void> updatePackageVersion(Long ID, Long packageID, String versionNumber) throws ValidatorException {
        Callable<Void> callable = () -> {
            this.packageVersionRepo.findOne(ID).orElseThrow(() -> new AppException("Non-existent package version with this ID !"));
            /*Contract.ensure(!samePackageIDDifferentID(ID,packageID), "A package version already exists for the given packageID.");*/

            var packageVersion = new PackageVersion(ID,packageID, versionNumber);
            this.packageVersionRepo.update(packageVersion);
            return null;
        };
        return this.executorService.submit(callable);
    }

    @Override
    public Future<Stream<PackageVersion>> getFilteredPackageVersions(String type, String input) {
        var packageVersions = this.packageVersionRepo.findAll();
        switch (type) {
            case "packageID":
                return this.executorService.submit(() ->StreamSupport.stream(packageVersions.spliterator(), false).filter(p -> p.getPackageID().toString().contains(input)));
            case "versionNumber":
                return this.executorService.submit(() ->StreamSupport.stream(packageVersions.spliterator(), false).filter(p -> p.getVersionNumber().contains(input)));
        }
        throw new AppException("This type does not exist !");
    }

    @Override
    public Future<Stream<PackageVersion>> getAllPackageVersions() {
        var packageVersions = this.packageVersionRepo.findAll();
        return this.executorService.submit(() ->StreamSupport.stream(packageVersions.spliterator(), false));
    }
}