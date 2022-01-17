package ro.arc.packageManager.client.ui;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Future;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import ro.arc.packageManager.client.service.MaintainerServiceClient;
import ro.arc.packageManager.client.service.PackageMaintainerServiceClient;
import ro.arc.packageManager.client.service.PackageServiceClient;
import ro.arc.packageManager.client.service.PackageVersionServiceClient;
import ro.arc.packageManager.common.domain.exceptions.AppException;

import static java.lang.Long.parseLong;

public final class TUI {
  private MaintainerServiceClient maintainerService;
  private PackageServiceClient packageService;
  private PackageMaintainerServiceClient packageMaintainerService;
  private PackageVersionServiceClient packageVersionService;

  private BufferedReader stdin = new BufferedReader(new InputStreamReader(System.in));
  private List<MappedFuture<?, String>> incompleteFuturesBuffer = new ArrayList<>();

  public TUI(
    MaintainerServiceClient maintainerService,
    PackageServiceClient packageService,
    PackageMaintainerServiceClient packageMaintainerService,
    PackageVersionServiceClient packageVersionService
  ) {
    this.maintainerService = maintainerService;
    this.packageService = packageService;
    this.packageMaintainerService = packageMaintainerService;
    this.packageVersionService = packageVersionService;
  }

  private String getLine() throws IOException {
    return stdin.readLine();
  }

  private void withMessage(Future<Void> future, String message) {
    incompleteFuturesBuffer.add(new MappedFuture<>(future, v -> message));
  }

  private <T> void printAll(Future<Stream<T>> future) {
    incompleteFuturesBuffer.add(
      new MappedFuture<>(
        future,
        ps -> ps.map(Object::toString).reduce("", (a, b) -> a + "\n" + b)));
  }

  private void addMaintainer() throws IOException {
    System.out.print("userName: ");
    var userName = getLine();

    System.out.print("fullName: ");
    var fullName = getLine();

    System.out.print("email: ");
    var email = getLine();

    withMessage(
      maintainerService.addMaintainer(userName, fullName, email),
      "Maintainer added successfully.");
  }

  private void deleteMaintainer() throws IOException {
    System.out.printf("ID : ");
    var maintainerIDString = getLine();
    var packageID = parseLong(maintainerIDString);

    withMessage(
      maintainerService.deleteMaintainer(packageID),
      "Maintainer deleted successfully.");
  }

  private void updateMaintainer() throws IOException {
    System.out.printf("ID : ");
    var maintainerIDString = getLine();
    var maintainerID = parseLong(maintainerIDString);

    System.out.print("userName: ");
    var userName = getLine();

    System.out.print("fullName: ");
    var fullName = getLine();

    System.out.print("email: ");
    var email = getLine();

    withMessage(
      maintainerService.updateMaintainer(maintainerID, userName, fullName, email),
      "Maintainer updated successfully.");
  }

  private void showFilteredMaintainer() throws IOException {
    System.out.println("Type can be : userName, fullName or email");
    System.out.printf("type : ");
    var type = getLine();

    System.out.printf("input : ");
    var input = getLine();

    printAll(maintainerService.showFilteredMaintainers(type, input));
  }

  private void addPackage() throws IOException {
    System.out.print("name: ");
    var name = getLine();

    System.out.print("description: ");
    var description = getLine();

    System.out.print("sourceRepo: ");
    var sourceRepo = getLine();

    System.out.print("license: ");
    var license = getLine();

    withMessage(
      packageService.addPackage(name, description, sourceRepo, license),
      "Package added successfully.");
  }

  private void deletePackage() throws IOException {
    System.out.printf("ID : ");
    var packageIDString = getLine();
    var packageID = parseLong(packageIDString);

    withMessage(
      packageService.deletePackage(packageID),
      "Package deleted successfully.");
  }

  private void updatePackage() throws IOException {
    System.out.printf("ID : ");
    var packageIDString = getLine();
    var packageID = parseLong(packageIDString);

    System.out.print("name: ");
    var name = getLine();

    System.out.print("description: ");
    var description = getLine();

    System.out.print("sourceRepo: ");
    var sourceRepo = getLine();

    System.out.print("license: ");
    var license = getLine();

    withMessage(
      packageService.updatePackage(packageID, name, description, sourceRepo, license),
      "Package updated successfully.");
  }

  private void showFilteredPackage() throws IOException {
    System.out.println("Type can be : name, description, sourceRepo or license\n");
    System.out.printf("type : ");
    var type = getLine();

    System.out.printf("input : ");
    var input = getLine();

    printAll(packageService.showFilteredPackages(type, input));
  }

  private void addPackageMaintainer() throws IOException {
    System.out.print("maintainerID: ");
    var maintainerIDString = getLine();
    var maintainerID = parseLong(maintainerIDString);

    System.out.print("packageID: ");
    var packageIDString = getLine();
    var packageID = parseLong(packageIDString);

    withMessage(
      packageMaintainerService.addPackageMaintainer(maintainerID, packageID),
      "PackageMaintainer added successfully.");
  }

  private void deletePackageMaintainer() throws IOException {
    System.out.printf("id: ");
    var idString = getLine();
    var id = parseLong(idString);

    withMessage(
      packageMaintainerService.deletePackageMaintainer(id),
      "PackageMaintainer deleted successfully.");
  }

  private void updatePackageMaintainer() throws IOException {
    System.out.printf("id: ");
    var idString = getLine();
    var id = parseLong(idString);

    System.out.print("maintainerID: ");
    var maintainerIDString = getLine();
    var maintainerID = parseLong(maintainerIDString);

    System.out.print("packageID: ");
    var packageIDString = getLine();
    var packageID = parseLong(packageIDString);

    withMessage(
      packageMaintainerService.updatePackageMaintainer(id, maintainerID, packageID),
      "PackageMaintainer updated successfully.");
  }

  private void showFilteredPackageMaintainers() throws IOException {
    System.out.printf("Field (maintainerID | packageID): ");
    var field = getLine();

    System.out.printf("Search term: ");
    var searchTerm = getLine();

    printAll(packageMaintainerService.showFilteredPackageMaintainers(field, searchTerm));
  }

  private void showMaintainers() throws IOException {
    printAll(maintainerService.showMaintainers());
  }

  private void showPackages() throws IOException {
    printAll(packageService.showPackages());
  }

  private void showPackageMaintainers() throws IOException {
    printAll(packageMaintainerService.showPackageMaintainers());
  }

  private void addPackageVersion() throws IOException {
    System.out.print("packageID: ");
    var packageID = getLine();

    System.out.print("versionNumber: ");
    var versionNumber = getLine();

    withMessage(
            packageVersionService.addPackageVersion(parseLong(packageID), versionNumber),
            "PackageVersion added successfully.");
  }
  private void updatePackageVersion() throws IOException{
    System.out.printf("ID : ");
    var packageVersionID = getLine();

    System.out.print("packageID: ");
    var packageID = getLine();

    System.out.print("versionNumber: ");
    var versionNumber = getLine();


    withMessage(
            packageVersionService.updatePackageVersion(parseLong(packageVersionID),parseLong(packageID),versionNumber),
            "PackageVersion updated successfully.");
  }

  private void deletePackageVersion() throws IOException{
    System.out.printf("ID: ");
    var packageVersionID = getLine();

    withMessage(
            packageVersionService.deletePackageVersion(parseLong(packageVersionID)),
            "PackageVersion deleted successfully.");
  }

  private void showPackageVersions() {
    printAll(packageVersionService.showPackageVersions());
  }

  private void showFilteredPackageVersions() throws IOException{
    System.out.printf("Field (packageID | versionNumber): ");
    var field = getLine();

    System.out.printf("Search term: ");
    var searchTerm = getLine();

    printAll(packageVersionService.showFilteredPackageVersions(field, searchTerm));
  }


  private void printMenu() {
    System.out.println("                                                All possible commands:                                                      exit");
    System.out.println("________________________________________________________________________________________________________________________________");
    System.out.println("|         Package:         |         Maintainer:         |         PackageMaintainer:         |         PackageVersion:         |");
    System.out.println("=================================================================================================================================");
    System.out.println("|  add package             |  add maintainer             |  add packageMaintainer             |  add packageVersion             |");
    System.out.println("|  update package          |  update maintainer          |  update packageMaintainer          |  update packageVersion          |");
    System.out.println("|  delete package          |  delete maintainer          |  delete packageMaintainer          |  delete packageVersion          |");
    System.out.println("|  show packages           |  show maintainers           |  show packageMaintainers           |  show packageVersions           |");
    System.out.println("|  show filtered packages  |  show filtered maintainers  |  show filtered packageMaintainers  |  show filtered packageVersions  |");
    System.out.println("‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾");
  }

  public void run() {
    printMenu();
    while (true) {
      try {
        var futures = incompleteFuturesBuffer.stream().collect(Collectors.partitioningBy(MappedFuture::isCompleted));
        incompleteFuturesBuffer = futures.get(false);

        futures.get(true)
          .stream()
          .map(MappedFuture::force)
          .forEach(System.out::println);

        System.out.print("\n╭─[packageManager]\n╰─λ ");
        var cmd = getLine();

        if (cmd == null) {
          System.out.println("EOF detected, quitting...");
          return;
        }

        switch (cmd) {
        case "exit":
          return;

        case "add maintainer":
          addMaintainer();
          break;
        case "add package":
          addPackage();
          break;
        case "add packageMaintainer":
          addPackageMaintainer();
          break;

        case "delete package":
          deletePackage();
          break;

        case "update package":
          updatePackage();
          break;

        case "delete maintainer":
          deleteMaintainer();
          break;
        case "update maintainer":
          updateMaintainer();
          break;
        case "show filtered maintainers":
          showFilteredMaintainer();
          break;
        case "show maintainers":
          showMaintainers();
          break;
        case "show packages":
          showPackages();
          break;
        case "show packageMaintainers":
          showPackageMaintainers();
          break;

        case "show filtered packages":
          showFilteredPackage();
          break;

        case "delete packageMaintainer":
          deletePackageMaintainer();
          break;

        case "update packageMaintainer":
          updatePackageMaintainer();
          break;

        case "show filtered packageMaintainers":
          showFilteredPackageMaintainers();
          break;

        case "add packageVersion":
          addPackageVersion();
          break;

        case "update packageVersion":
          updatePackageVersion();
          break;

        case "delete packageVersion":
          deletePackageVersion();
          break;

        case "show packageVersions":
          showPackageVersions();
          break;

        case "show filtered packageVersions":
          showFilteredPackageVersions();
          break;

          case "":
          break;

        default:
          System.out.println("Command not recognized.");
          break;
        }
      } catch (IOException e) {
        System.out.println("Error reading from stdin. Exception details:");
        e.printStackTrace();
      } catch (NumberFormatException e) {
        System.out.println("Error: Expected a number.");
      } catch (AppException e) {
        System.out.println("Error: " + e.getMessage());
      }
    }
  }
}
