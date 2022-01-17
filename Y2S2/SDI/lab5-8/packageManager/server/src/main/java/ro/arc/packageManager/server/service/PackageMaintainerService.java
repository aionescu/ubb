package ro.arc.packageManager.server.service;

import ro.arc.packageManager.common.domain.Maintainer;
import ro.arc.packageManager.common.domain.Package;
import ro.arc.packageManager.common.domain.PackageMaintainer;
import ro.arc.packageManager.common.domain.exceptions.AppException;
import ro.arc.packageManager.common.domain.exceptions.Contract;
import ro.arc.packageManager.common.domain.validators.ValidatorException;
import ro.arc.packageManager.common.service.IPackageMaintainerService;
import ro.arc.packageManager.server.repository.Repository;

import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

public class PackageMaintainerService implements IPackageMaintainerService {
    private Repository<Long, Maintainer> maintainerRepo;
    private Repository<Long, Package> packageRepo;
    private Repository<Long, PackageMaintainer> packageMaintainerRepo;
    private ExecutorService executorService;

    public PackageMaintainerService(
            Repository<Long, Maintainer> maintainerRepo,
            Repository<Long, Package> packageRepo,
            Repository<Long, PackageMaintainer> packageMaintainerRepo,
            ExecutorService executorService)
    {
        this.maintainerRepo = maintainerRepo;
        this.packageRepo = packageRepo;
        this.packageMaintainerRepo = packageMaintainerRepo;
        this.executorService = executorService;
    }

    @Override
    public Future<Void> addPackageMaintainer(Long maintainerID, Long packageID) throws ValidatorException, AppException {
        Callable<Void> callable = () -> {
            maintainerRepo.findOne(maintainerID).orElseThrow(() -> new AppException("Inexistent maintainer."));
            packageRepo.findOne(packageID).orElseThrow(() -> new AppException("Inexistent package."));

            Contract.ensure(!existsPackageMaintainer(maintainerID, packageID), "That package already has that maintainer.");

            var packageMaintainer = new PackageMaintainer(0l, maintainerID, packageID);
            this.packageMaintainerRepo.save(packageMaintainer);
            return null;
        };
        return this.executorService.submit(callable);
    }

    private boolean existsPackageMaintainer(Long maintainerID, Long packageID) {
        Contract.notNull(maintainerID, "maintainerID");
        Contract.notNull(packageID, "packageID");

        var all = this.packageMaintainerRepo.findAll();

        return
                StreamSupport.stream(all.spliterator(), false)
                        .anyMatch(pm -> pm.getMaintainerID().equals(maintainerID) && pm.getPackageID().equals(packageID));
    }

    @Override
    public Future<Void> deletePackageMaintainer(Long id) {
        Callable<Void> callable = () -> {
            this.packageMaintainerRepo.findOne(id).orElseThrow(() -> new AppException("No PackageMaintainer exists with this ID."));
            this.packageMaintainerRepo.delete(id);
            return null;
        };
        return this.executorService.submit(callable);
    }

    @Override
    public Future<Void> updatePackageMaintainer(Long id, Long maintainerID, Long packageID) throws ValidatorException {
        Callable<Void> callable = () -> {
            this.packageMaintainerRepo.findOne(id).orElseThrow(() -> new AppException("No PackageMaintainer exists with this ID."));
            Contract.ensure(!existsPackageMaintainer(maintainerID, packageID), "A PackageMaintainer with the new data already exists.");

            var pkgMan = new PackageMaintainer(id, maintainerID, packageID);
            this.packageMaintainerRepo.update(pkgMan);
            return null;
        };
        return this.executorService.submit(callable);
    }

    @Override
    public Future<Stream<PackageMaintainer>> getFilteredPackageMaintainers(String field, String searchTerm) {
        var entities = this.packageMaintainerRepo.findAll();
        var packageMaintainters = StreamSupport.stream(entities.spliterator(), false);

        switch (field) {
            case "maintainerID":
                return this.executorService.submit(() ->packageMaintainters.filter(p -> String.valueOf(p.getMaintainerID()).contains(searchTerm)));
            case "packageID":
                return this.executorService.submit(() ->packageMaintainters.filter(p -> String.valueOf(p.getPackageID()).contains(searchTerm)));
            default:
                throw new AppException("This type does not exist !");
        }
    }

    @Override
    public Future<Stream<PackageMaintainer>> getAllPackageMaintainers() {
        var packageMaintainers = this.packageMaintainerRepo.findAll();
        return this.executorService.submit(() ->StreamSupport.stream(packageMaintainers.spliterator(), false));
    }
}
