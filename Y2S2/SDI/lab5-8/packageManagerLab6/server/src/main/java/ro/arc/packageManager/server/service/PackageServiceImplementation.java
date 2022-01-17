package ro.arc.packageManager.server.service;

import ro.arc.packageManager.common.domain.Package;
import ro.arc.packageManager.common.domain.exceptions.AppException;
import ro.arc.packageManager.common.domain.exceptions.Contract;
import ro.arc.packageManager.common.domain.validators.ValidatorException;
import ro.arc.packageManager.common.service.PackageService;
import ro.arc.packageManager.server.repository.Repository;

import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

public class PackageServiceImplementation implements PackageService {

    private Repository<Long, Package> packageRepo;

    public PackageServiceImplementation(Repository<Long, Package> packageRepo) {
        this.packageRepo = packageRepo;
    }

    @Override
    public void addPackage(Package pkg) throws ValidatorException {
        Contract.ensure(!existsPackageName(pkg.getName()), "A package with that name already exists.");

        this.packageRepo.save(pkg);
    }

    private boolean existsPackageName(String name) {
        Contract.notNull(name, "name");

        var all = packageRepo.findAll();

        return StreamSupport.stream(all.spliterator(), false).anyMatch(m -> m.getName().equals(name));
    }

    private boolean existPackageNameDifferentID(String name, Long ID)
    {
        Contract.notNull(name, "name");

        var all = packageRepo.findAll();

        return StreamSupport.stream(all.spliterator(), false).anyMatch(m -> m.getName().equals(name) && m.getID() != ID);
    }

    @Override
    public void deletePackage(Long ID){
        this.packageRepo.findOne(ID).orElseThrow(() -> new AppException("Non-existent package with this ID !"));
        this.packageRepo.delete(ID);
    }

    @Override
    public void updatePackage(Package pkg) throws ValidatorException {
        this.packageRepo.findOne(pkg.getID()).orElseThrow(() -> new AppException("Non-existent package with this ID !"));
        Contract.ensure(!existPackageNameDifferentID(pkg.getName(), pkg.getID()), "A package with the new name already exists.");

        this.packageRepo.update(pkg);
    }

    @Override
    public List<Package> getFilteredPackages(String type, String input) {
        var packages = this.packageRepo.findAll();
        switch (type) {
            case "name":
                return StreamSupport.stream(packages.spliterator(), false).filter(p -> p.getName().contains(input)).collect(Collectors.toList());
            case "description":
                return StreamSupport.stream(packages.spliterator(), false).filter(p -> p.getDescription().contains(input)).collect(Collectors.toList());
            case "sourceRepo":
                return StreamSupport.stream(packages.spliterator(), false).filter(p -> p.getSourceRepo().contains(input)).collect(Collectors.toList());
            case "license":
                return StreamSupport.stream(packages.spliterator(), false).filter(p -> p.getLicense().contains(input)).collect(Collectors.toList());
        }
        throw new AppException("This type does not exist !");
    }

    @Override
    public List<Package> getAllPackages() {
        var packages = this.packageRepo.findAll();
        return StreamSupport.stream(packages.spliterator(), false).collect(Collectors.toList());
    }
}
