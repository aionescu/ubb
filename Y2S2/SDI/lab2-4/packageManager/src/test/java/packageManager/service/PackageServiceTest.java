package packageManager.service;

import java.util.*;
import java.util.stream.Collectors;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.*;

import ro.arc.packageManager.domain.Package;
import ro.arc.packageManager.domain.exceptions.AppException;
import ro.arc.packageManager.domain.validators.PackageValidator;
import ro.arc.packageManager.repository.InMemoryRepository;
import ro.arc.packageManager.repository.Repository;
import ro.arc.packageManager.service.PackageService;

public class PackageServiceTest {
  private Repository<Long, Package> packageRepo;
  private PackageValidator PackageValidator = new PackageValidator();
  private PackageService packageService;

  @Before
  public void setUp() throws Exception {
    packageRepo = new InMemoryRepository<>(PackageValidator);
    packageService = new PackageService(packageRepo);
  }

  @After
  public void tearDown() throws Exception {
    packageRepo = null;
    packageService = null;
  }

  @Test
  public void addPackage() {
    @SuppressWarnings("unused")
    var pkg = new Package(1l, "name1", "description1", "https://source1.com", "license1");
    packageService.addPackage("name1", "description1", "https://source1.com", "license1");
    assertEquals("Package should be in repo", packageService.getAllPackages().count(), 1);
  }

  @Test
  public void getAllPackages() {
    Package package1 = new Package(0l, "name1", "description1", "https://source1.com", "license1");
    Package package2 = new Package(1l, "name2", "description2", "https://source2.com", "license2");
    packageService.addPackage("name1", "description1", "https://source1.com", "license1");
    packageService.addPackage("name2", "description2", "https://source2.com", "license2");
    HashSet<Package> hashSet = new HashSet<Package>();
    hashSet.add(package1);
    hashSet.add(package2);
    assertEquals("Packages should be equal",
      packageService.getAllPackages().collect(Collectors.toCollection(HashSet::new)), hashSet);
  }

  @Test
  public void testDeletePackage_ExistentPackage() {
    Package pkg1 = new Package(1l, "name2", "description2", "https://source2.com", "license2");
    packageService.addPackage("name1", "description1", "https://source1.com", "license1");
    packageService.addPackage("name2", "description2", "https://source2.com", "license2");
    packageService.deletePackage(0l);
    HashSet<Package> hashSet = new HashSet<Package>();
    hashSet.add(pkg1);
    assertEquals("Packages should be equal",
      packageService.getAllPackages().collect(Collectors.toCollection(HashSet::new)), hashSet);
  }

  @Test(expected = AppException.class)
  public void testDeletePackage_NonexistentPackage() {
    packageService.addPackage("name1", "description1", "https://source1.com", "license1");
    packageService.addPackage("name2", "description2", "https://source2.com", "license2");
    packageService.deletePackage(2l);
  }

  @Test
  public void testUpdatePackage_ExistentPackage() {
    packageService.addPackage("name1", "description1", "https://source1.com", "license1");
    packageService.updatePackage(0l, "newName", "newDescription", "https://new.source.com", "newLicense");
    ArrayList<Package> pkgs = packageService.getAllPackages().collect(Collectors.toCollection(ArrayList::new));
    assertEquals("Names should be equal", pkgs.get(0).getName(), "newName");
  }

  @Test(expected = AppException.class)
  public void testUpdatePackage_NonexistentPackage() {
    packageService.addPackage("name1", "description1", "https://source1.com", "license1");
    packageService.updatePackage(1l, "newName", "newDescription", "https://new.source.com", "newLicense");
  }

  @Test(expected = AppException.class)
  public void testUpdatePackage_NewNameAlreadyUsed() {
    packageService.addPackage("name1", "description1", "https://source1.com", "license1");
    packageService.addPackage("name2", "description2", "https://source2.com", "license2");
    packageService.updatePackage(0l, "name2", "defe", "https://dadsa.com", "dasda");
  }

  @Test
  public void testGetFilteredPackages() {
    packageService.addPackage("name1", "description1", "https://source1.com", "license1");
    packageService.addPackage("dsasd", "description2", "https://source2.com", "license2");

    ArrayList<Package> pkgs =
      packageService
      .getFilteredPackages("name", "sa")
      .collect(Collectors.toCollection(ArrayList::new));

    assertEquals("Should be just one element", pkgs.size(), 1);
    assertEquals("Names should be equal", pkgs.get(0).getName(), "dsasd");
  }
}
