package ro.arc.packageManager.service;

import ro.arc.packageManager.domain.Maintainer;
import ro.arc.packageManager.domain.Package;
import ro.arc.packageManager.domain.PackageMaintainer;
import ro.arc.packageManager.domain.exceptions.AppException;
import ro.arc.packageManager.domain.exceptions.Contract;
import ro.arc.packageManager.domain.validators.ValidatorException;
import ro.arc.packageManager.repository.Repository;

import java.util.stream.Stream;
import java.util.stream.StreamSupport;

public class PackageMaintainerService {
  private Repository<Long, Maintainer> maintainerRepo;
  private Repository<Long, Package> packageRepo;
  private Repository<Long, PackageMaintainer> packageMaintainerRepo;

  public PackageMaintainerService(
    Repository<Long, Maintainer> maintainerRepo,
    Repository<Long, Package> packageRepo,
    Repository<Long, PackageMaintainer> packageMaintainerRepo)
  {
    this.maintainerRepo = maintainerRepo;
    this.packageRepo = packageRepo;
    this.packageMaintainerRepo = packageMaintainerRepo;
  }

  public void addPackageMaintainer(Long maintainerID, Long packageID) throws ValidatorException, AppException {
    maintainerRepo.findOne(maintainerID).orElseThrow(() -> new AppException("Inexistent maintainer."));
    packageRepo.findOne(packageID).orElseThrow(() -> new AppException("Inexistent package."));

    Contract.ensure(!existsPackageMaintainer(maintainerID, packageID), "That package already has that maintainer.");

    var packageMaintainer = new PackageMaintainer(0l, maintainerID, packageID);
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

  public void deletePackageMaintainer(Long id) {
    this.packageMaintainerRepo.findOne(id).orElseThrow(() -> new AppException("No PackageMaintainer exists with this ID."));
    this.packageMaintainerRepo.delete(id);
  }

  public void updatePackageMaintainer(Long id, Long maintainerID, Long packageID) throws ValidatorException {
    this.packageMaintainerRepo.findOne(id).orElseThrow(() -> new AppException("No PackageMaintainer exists with this ID."));
    Contract.ensure(!existsPackageMaintainer(maintainerID, packageID), "A PackageMaintainer with the new data already exists.");

    var pkgMan = new PackageMaintainer(id, maintainerID, packageID);
    this.packageMaintainerRepo.update(pkgMan);
  }

  public Stream<PackageMaintainer> getFilteredPackageMaintainers(String field, String searchTerm) {
    var entities = this.packageMaintainerRepo.findAll();
    var packageMaintainters = StreamSupport.stream(entities.spliterator(), false);

    switch (field) {
      case "maintainerID":
        return packageMaintainters.filter(p -> String.valueOf(p.getMaintainerID()).contains(searchTerm));
      case "packageID":
        return packageMaintainters.filter(p -> String.valueOf(p.getPackageID()).contains(searchTerm));
      default:
        throw new AppException("This type does not exist !");
    }
  }

  public Stream<PackageMaintainer> getAllPackageMaintainers() {
    var packageMaintainers = this.packageMaintainerRepo.findAll();
    return StreamSupport.stream(packageMaintainers.spliterator(), false);
  }
}
