package ro.arc.packageManager.service;

import ro.arc.packageManager.domain.Package;
import ro.arc.packageManager.domain.exceptions.AppException;
import ro.arc.packageManager.domain.exceptions.Contract;
import ro.arc.packageManager.domain.validators.ValidatorException;
import ro.arc.packageManager.repository.Repository;

import java.util.stream.Stream;
import java.util.stream.StreamSupport;

public class PackageService {
  private Repository<Long, Package> packageRepo;

  public PackageService(Repository<Long, Package> packageRepo) {
    this.packageRepo = packageRepo;
  }

  public void addPackage(String name, String description, String sourceRepo, String license) throws ValidatorException {
    Contract.ensure(!existsPackageName(name), "A package with that name already exists.");

    var pkg = new Package(0l, name, description, sourceRepo, license);
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

  public void deletePackage(Long ID){
    this.packageRepo.findOne(ID).orElseThrow(() -> new AppException("Non-existent package with this ID !"));
    this.packageRepo.delete(ID);
  }

  public void updatePackage(Long ID, String name, String description, String sourceRepo, String license) throws ValidatorException {
    this.packageRepo.findOne(ID).orElseThrow(() -> new AppException("Non-existent package with this ID !"));
    Contract.ensure(!existPackageNameDifferentID(name, ID), "A package with the new name already exists.");

    var pkg = new Package(ID, name, description, sourceRepo, license);
    this.packageRepo.update(pkg);
  }

  public Stream<Package> getFilteredPackages(String type, String input) {
    var packages = this.packageRepo.findAll();
    switch (type) {
      case "name":
        return StreamSupport.stream(packages.spliterator(), false).filter(p -> p.getName().contains(input));
      case "description":
        return StreamSupport.stream(packages.spliterator(), false).filter(p -> p.getDescription().contains(input));
      case "sourceRepo":
        return StreamSupport.stream(packages.spliterator(), false).filter(p -> p.getSourceRepo().contains(input));
      case "license":
        return StreamSupport.stream(packages.spliterator(), false).filter(p -> p.getLicense().contains(input));
    }
    throw new AppException("This type does not exist !");
  }

  public Stream<Package> getAllPackages() {
    var packages = this.packageRepo.findAll();
    return StreamSupport.stream(packages.spliterator(), false);
  }
}
