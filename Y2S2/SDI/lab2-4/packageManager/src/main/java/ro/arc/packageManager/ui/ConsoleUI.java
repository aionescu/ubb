package ro.arc.packageManager.ui;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

import ro.arc.packageManager.domain.exceptions.AppException;
import ro.arc.packageManager.service.MaintainerService;
import ro.arc.packageManager.service.PackageService;
import ro.arc.packageManager.service.PackageMaintainerService;

public class ConsoleUI {
  private MaintainerService maintainerService;
  private PackageService packageService;
  private PackageMaintainerService packageMaintainerService;
  private BufferedReader stdin;

  public ConsoleUI(
    MaintainerService maintainerService,
    PackageService packageService,
    PackageMaintainerService packageMaintainerService)
  {
    this.maintainerService = maintainerService;
    this.packageService = packageService;
    this.packageMaintainerService = packageMaintainerService;
    stdin = new BufferedReader(new InputStreamReader(System.in));
  }

  private String getLine() throws IOException {
    return stdin.readLine();
  }

  private void addMaintainer() throws IOException {
    System.out.print("userName: ");
    var userName = getLine();

    System.out.print("fullName: ");
    var fullName = getLine();

    System.out.print("email: ");
    var email = getLine();

    maintainerService.addMaintainer(userName, fullName, email);
  }

  private void deleteMaintainer() throws IOException {
    System.out.printf("ID : ");
    var maintainerIDString = getLine();
    var packageID = Long.parseLong(maintainerIDString);

    maintainerService.deleteMaintainer(packageID);
  }

  private void updateMaintainer() throws IOException {
    System.out.printf("ID : ");
    var maintainerIDString = getLine();
    var maintainerID = Long.parseLong(maintainerIDString);

    System.out.print("userName: ");
    var userName = getLine();

    System.out.print("fullName: ");
    var fullName = getLine();

    System.out.print("email: ");
    var email = getLine();


    maintainerService.updateMaintainer(maintainerID, userName, fullName,email);
  }

  private void showFilteredMaintainer() throws IOException {
    System.out.println("Type can be : userName, fullName or email");
    System.out.printf("type : ");
    var type = getLine();

    System.out.printf("input : ");
    var input = getLine();

    maintainerService.getFilteredMaintainers(type, input).forEach(System.out::println);
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

    packageService.addPackage(name, description, sourceRepo, license);
  }

  private void deletePackage() throws IOException {
    System.out.printf("ID : ");
    var packageIDString = getLine();
    var packageID = Long.parseLong(packageIDString);

    packageService.deletePackage(packageID);
  }

  private void updatePackage() throws IOException {
    System.out.printf("ID : ");
    var packageIDString = getLine();
    var packageID = Long.parseLong(packageIDString);

    System.out.print("name: ");
    var name = getLine();

    System.out.print("description: ");
    var description = getLine();

    System.out.print("sourceRepo: ");
    var sourceRepo = getLine();

    System.out.print("license: ");
    var license = getLine();

    packageService.updatePackage(packageID, name, description, sourceRepo, license);
  }

  private void showFilteredPackage() throws IOException {
    System.out.println("Type can be : name, description, sourceRepo or license\n");
    System.out.printf("type : ");
    var type = getLine();

    System.out.printf("input : ");
    var input = getLine();

    packageService.getFilteredPackages(type, input).forEach(System.out::println);
  }

  private void addPackageMaintainer() throws IOException {
    System.out.print("maintainerID: ");
    var maintainerIDString = getLine();
    var maintainerID = Long.parseLong(maintainerIDString);

    System.out.print("packageID: ");
    var packageIDString = getLine();
    var packageID = Long.parseLong(packageIDString);

    packageMaintainerService.addPackageMaintainer(maintainerID, packageID);
  }

  private void deletePackageMaintainer() throws IOException {
    System.out.printf("id: ");
    var idString = getLine();
    var id = Long.parseLong(idString);

    packageMaintainerService.deletePackageMaintainer(id);
  }

  private void updatePackageMaintainer() throws IOException {
    System.out.printf("id: ");
    var idString = getLine();
    var id = Long.parseLong(idString);

    System.out.print("maintainerID: ");
    var maintainerIDString = getLine();
    var maintainerID = Long.parseLong(maintainerIDString);

    System.out.print("packageID: ");
    var packageIDString = getLine();
    var packageID = Long.parseLong(packageIDString);

    packageMaintainerService.updatePackageMaintainer(id, maintainerID, packageID);
  }

  private void showFilteredPackageMaintainers() throws IOException {
    System.out.printf("Field (maintainerID | packageID): ");
    var field = getLine();

    System.out.printf("Search term: ");
    var searchTerm = getLine();

    packageMaintainerService
      .getFilteredPackageMaintainers(field, searchTerm)
      .forEach(System.out::println);
  }

  private void showMaintainers() throws IOException {
    maintainerService.getAllMaintainers().forEach(System.out::println);
  }

  private void showPackages() throws IOException {
    packageService.getAllPackages().forEach(System.out::println);
  }

  private void showPackageMaintainers() throws IOException {
    packageMaintainerService.getAllPackageMaintainers().forEach(System.out::println);
  }

  public void run() {
    while (true) {
      try {
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
          case "add package-maintainer":
            addPackageMaintainer();
            break;

          case "delete package" :
            deletePackage();
            break;

          case "update package" :
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
          case "show package-maintainers":
            showPackageMaintainers();
            break;

          case "show filtered packages" :
            showFilteredPackage();
            break;

          case "delete package-maintainer":
            deletePackageMaintainer();
            break;

          case "update package-maintainer":
            updatePackageMaintainer();
            break;

          case "show filtered package-maintainers":
            showFilteredPackageMaintainers();
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
