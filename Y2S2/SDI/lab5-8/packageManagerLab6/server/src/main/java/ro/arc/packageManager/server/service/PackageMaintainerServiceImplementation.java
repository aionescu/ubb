package ro.arc.packageManager.server.service;

import ro.arc.packageManager.common.domain.Maintainer;
import ro.arc.packageManager.common.domain.Package;
import ro.arc.packageManager.common.domain.PackageMaintainer;
import ro.arc.packageManager.common.domain.exceptions.AppException;
import ro.arc.packageManager.common.domain.exceptions.Contract;
import ro.arc.packageManager.common.domain.validators.ValidatorException;
import ro.arc.packageManager.common.service.PackageMaintainerService;
import ro.arc.packageManager.server.repository.Repository;

import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

public class PackageMaintainerServiceImplementation implements PackageMaintainerService {
    private Repository<Long, Maintainer> maintainerRepo;
    private Repository<Long, Package> packageRepo;
    private Repository<Long, PackageMaintainer> packageMaintainerRepo;

    public PackageMaintainerServiceImplementation(
            Repository<Long, Maintainer> maintainerRepo,
            Repository<Long, Package> packageRepo,
            Repository<Long, PackageMaintainer> packageMaintainerRepo)
    {
        this.maintainerRepo = maintainerRepo;
        this.packageRepo = packageRepo;
        this.packageMaintainerRepo = packageMaintainerRepo;
    }

    @Override
    public void addPackageMaintainer(PackageMaintainer packageMaintainer) throws ValidatorException, AppException {
        maintainerRepo.findOne(packageMaintainer.getMaintainerID()).orElseThrow(() -> new AppException("Inexistent maintainer."));
        packageRepo.findOne(packageMaintainer.getPackageID()).orElseThrow(() -> new AppException("Inexistent package."));

        Contract.ensure(!existsPackageMaintainer(packageMaintainer.getMaintainerID(), packageMaintainer.getPackageID()), "That package already has that maintainer.");

        this.packageMaintainerRepo.save(packageMaintainer);
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
    public void deletePackageMaintainer(Long id) {
        this.packageMaintainerRepo.findOne(id).orElseThrow(() -> new AppException("No PackageMaintainer exists with this ID."));
        this.packageMaintainerRepo.delete(id);
    }

    @Override
    public void updatePackageMaintainer(PackageMaintainer packageMaintainer) throws ValidatorException {
        this.packageMaintainerRepo.findOne(packageMaintainer.getPackageID()).orElseThrow(() -> new AppException("No PackageMaintainer exists with this ID."));
        Contract.ensure(!existsPackageMaintainer(packageMaintainer.getMaintainerID(), packageMaintainer.getMaintainerID()), "A PackageMaintainer with the new data already exists.");

        this.packageMaintainerRepo.update(packageMaintainer);
    }

    @Override
    public List<PackageMaintainer> getFilteredPackageMaintainers(String field, String searchTerm) {
        var entities = this.packageMaintainerRepo.findAll();
        var packageMaintainters = StreamSupport.stream(entities.spliterator(), false);

        switch (field) {
            case "maintainerID":
                return packageMaintainters.filter(p -> String.valueOf(p.getMaintainerID()).contains(searchTerm)).collect(Collectors.toList());
            case "packageID":
                return packageMaintainters.filter(p -> String.valueOf(p.getPackageID()).contains(searchTerm)).collect(Collectors.toList());
            default:
                throw new AppException("This type does not exist !");
        }
    }

    @Override
    public List<PackageMaintainer> getAllPackageMaintainers() {
        var packageMaintainers = this.packageMaintainerRepo.findAll();
        return StreamSupport.stream(packageMaintainers.spliterator(), false).collect(Collectors.toList());
    }
}
