package packageManager.service;

import java.util.*;
import java.util.stream.Collectors;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.*;

import ro.arc.packageManager.domain.Maintainer;
import ro.arc.packageManager.domain.Package;
import ro.arc.packageManager.domain.PackageMaintainer;
import ro.arc.packageManager.domain.exceptions.AppException;
import ro.arc.packageManager.domain.validators.MaintainerValidator;
import ro.arc.packageManager.domain.validators.PackageMaintainerValidator;
import ro.arc.packageManager.domain.validators.PackageValidator;
import ro.arc.packageManager.repository.InMemoryRepository;
import ro.arc.packageManager.repository.Repository;
import ro.arc.packageManager.service.PackageMaintainerService;

public class PackageMaintainerServiceTest {
  private Repository<Long, PackageMaintainer> packageMaintainerRepo;
  private PackageMaintainerValidator packageMaintainerValidator = new PackageMaintainerValidator();
  private PackageMaintainerService packageMaintainerService;

  private Repository<Long, Package> packageRepo;
  private PackageValidator packageValidator = new PackageValidator();

  private Repository<Long, Maintainer> maintainerRepo;
  private MaintainerValidator maintainerValidator = new MaintainerValidator();

  @Before
  public void setUp() throws Exception {
    packageMaintainerRepo = new InMemoryRepository<>(packageMaintainerValidator);
    maintainerRepo = new InMemoryRepository<>(maintainerValidator);
    packageRepo = new InMemoryRepository<>(packageValidator);
    packageMaintainerService = new PackageMaintainerService(maintainerRepo, packageRepo, packageMaintainerRepo);
  }

  @After
  public void tearDown() throws Exception {
    packageMaintainerRepo = null;
    maintainerRepo = null;
    packageRepo = null;
  }

  @Test
  public void addPackageMaintainer() {
    Package package1 = new Package(1l, "name1", "description1", "https://source1.com", "license1");
    packageRepo.save(package1);

    Maintainer maintainer1 = new Maintainer(1l, "userName1", "fullName1", "e@mail.com");
    maintainerRepo.save(maintainer1);

    @SuppressWarnings("unused")
    PackageMaintainer packageMaintainer = new PackageMaintainer(1l, 1l, 1l);
    packageMaintainerService.addPackageMaintainer(1l, 1l);

    assertEquals("Maintainer should be in repo", packageMaintainerService.getAllPackageMaintainers().count(), 1);
  }

  @Test
  public void getAllPackageMaintainers() {
    Package package1 = new Package(1l, "name1", "description1", "https://source1.com", "license1");
    Package package2 = new Package(2l, "name2", "description2", "https://source2.com", "license2");
    packageRepo.save(package1);
    packageRepo.save(package2);

    Maintainer maintainer1 = new Maintainer(1l, "userName1", "fullName1", "e1@mail.com");
    Maintainer maintainer2 = new Maintainer(2l, "userName2", "fullName2", "e1@mail.com");
    maintainerRepo.save(maintainer1);
    maintainerRepo.save(maintainer2);

    PackageMaintainer packageMaintainer1 = new PackageMaintainer(0l, 1l, 1l);
    PackageMaintainer packageMaintainer2 = new PackageMaintainer(1l, 2l, 2l);
    packageMaintainerService.addPackageMaintainer(1l, 1l);
    packageMaintainerService.addPackageMaintainer(2l, 2l);

    HashSet<PackageMaintainer> hashSet = new HashSet<PackageMaintainer>();
    hashSet.add(packageMaintainer1);
    hashSet.add(packageMaintainer2);

    assertEquals("PackageMaintainers should be equal",
        packageMaintainerService.getAllPackageMaintainers().collect(Collectors.toCollection(HashSet::new)), hashSet);
  }

  @Test
  public void testDeleteExistingPackageMainainer() {
    var pkg = new Package(0l, "pkg", "desc", "https://source.com", "license");
    var maintainer = new Maintainer(0l, "userName", "fullName", "e@mail.com");

    packageRepo.save(pkg);
    maintainerRepo.save(maintainer);

    packageMaintainerService.addPackageMaintainer(0l, 0l);
    packageMaintainerService.deletePackageMaintainer(0l);

    assertEquals("Repo should be empty.", packageMaintainerService.getAllPackageMaintainers().count(), 0);
  }

  @Test(expected = AppException.class)
  public void testDeleteInexistentPackageMainainer() {
    packageMaintainerService.deletePackageMaintainer(0l);
  }

  @Test
  public void testUpdateExistingPackageMaintainer() {
    packageRepo.save(new Package(0l, "pkg", "desc", "https://source.com", "license"));

    maintainerRepo.save(new Maintainer(0l, "userName", "fullName", "e@mail.com"));
    maintainerRepo.save(new Maintainer(1l, "userName2", "fullName2", "e2@mail.com"));

    packageMaintainerService.addPackageMaintainer(0l, 0l);
    packageMaintainerService.updatePackageMaintainer(0l, 1l, 0l);

    var pms = packageMaintainerService.getAllPackageMaintainers().collect(Collectors.toCollection(ArrayList::new));
    long maintainerID = pms.get(0).getMaintainerID();

    assertEquals("Maintainer's ID should be 1.", maintainerID, 1l);
  }

  @Test(expected = AppException.class)
  public void testUpdateInexistentPackageMaintainer() {
    packageMaintainerService.updatePackageMaintainer(0l, 0l, 0l);
  }

  @Test
  public void testGetFilteredPackages() {
    packageRepo.save(new Package(0l, "pkg", "desc", "https://source.com", "license"));

    maintainerRepo.save(new Maintainer(0l, "userName", "fullName", "e@mail.com"));
    maintainerRepo.save(new Maintainer(1l, "userName2", "fullName2", "e2@mail.com"));

    packageMaintainerService.addPackageMaintainer(0l, 0l);
    packageMaintainerService.addPackageMaintainer(1l, 0l);

    var pms =
      packageMaintainerService
      .getFilteredPackageMaintainers("maintainerID", "1")
      .collect(Collectors.toCollection(ArrayList::new));

    assertEquals("Should be just one PackageMaintainer.", pms.size(), 1);
  }
}
