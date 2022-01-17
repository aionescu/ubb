package ro.arc.packageManager.server.service;

import ro.arc.packageManager.common.domain.Package;
import ro.arc.packageManager.common.domain.exceptions.AppException;
import ro.arc.packageManager.common.domain.exceptions.Contract;
import ro.arc.packageManager.common.domain.validators.ValidatorException;
import ro.arc.packageManager.common.service.IPackageService;
import ro.arc.packageManager.server.repository.Repository;

import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

public class PackageService implements IPackageService {

    private Repository<Long, Package> packageRepo;
    private ExecutorService executorService;

    public PackageService(Repository<Long, Package> packageRepo, ExecutorService executorService) {
        this.packageRepo = packageRepo;
        this.executorService = executorService;
    }

    @Override
    public Future<Void> addPackage(String name, String description, String sourceRepo, String license) throws ValidatorException {
        Callable<Void> callable = () -> {
            Contract.ensure(!existsPackageName(name), "A package with that name already exists.");

            var pkg = new Package(0l, name, description, sourceRepo, license);
            this.packageRepo.save(pkg);
            return null;
        };
        return this.executorService.submit(callable);
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
    public Future<Void> deletePackage(Long ID){
        Callable<Void> callable = () -> {
            this.packageRepo.findOne(ID).orElseThrow(() -> new AppException("Non-existent package with this ID !"));
            this.packageRepo.delete(ID);
            return null;
        };
        return this.executorService.submit(callable);
    }

    @Override
    public Future<Void> updatePackage(Long ID, String name, String description, String sourceRepo, String license) throws ValidatorException {
        Callable<Void> callable = () -> {
            this.packageRepo.findOne(ID).orElseThrow(() -> new AppException("Non-existent package with this ID !"));
            Contract.ensure(!existPackageNameDifferentID(name, ID), "A package with the new name already exists.");

            var pkg = new Package(ID, name, description, sourceRepo, license);
            this.packageRepo.update(pkg);
            return null;
        };
        return this.executorService.submit(callable);
    }

    @Override
    public Future<Stream<Package>> getFilteredPackages(String type, String input) {
        var packages = this.packageRepo.findAll();
        switch (type) {
            case "name":
                return this.executorService.submit(() ->StreamSupport.stream(packages.spliterator(), false).filter(p -> p.getName().contains(input)));
            case "description":
                return this.executorService.submit(() ->StreamSupport.stream(packages.spliterator(), false).filter(p -> p.getDescription().contains(input)));
            case "sourceRepo":
                return this.executorService.submit(() ->StreamSupport.stream(packages.spliterator(), false).filter(p -> p.getSourceRepo().contains(input)));
            case "license":
                return this.executorService.submit(() ->StreamSupport.stream(packages.spliterator(), false).filter(p -> p.getLicense().contains(input)));
        }
        throw new AppException("This type does not exist !");
    }

    @Override
    public Future<Stream<Package>> getAllPackages() {
        var packages = this.packageRepo.findAll();
        return this.executorService.submit(() ->StreamSupport.stream(packages.spliterator(), false));
    }
}
