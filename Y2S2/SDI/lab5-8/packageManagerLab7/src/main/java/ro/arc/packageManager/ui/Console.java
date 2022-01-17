package ro.arc.packageManager.ui;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import ro.arc.packageManager.domain.Maintainer;
import ro.arc.packageManager.domain.Package;
import ro.arc.packageManager.domain.PackageMaintainer;
import ro.arc.packageManager.domain.PackageVersion;
import ro.arc.packageManager.domain.exceptions.AppException;
import ro.arc.packageManager.service.MaintainerService;
import ro.arc.packageManager.service.PackageMaintainerService;
import ro.arc.packageManager.service.PackageService;
import ro.arc.packageManager.service.PackageVersionService;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Map;

import static java.util.Map.entry;

@Component
public class Console {
  public static final String RESET = "\u001B[0m";

  public static final String BLACK = "\u001B[30m";
  public static final String RED = "\u001B[31m";
  public static final String GREEN = "\u001B[32m";
  public static final String YELLOW = "\u001B[33m";
  public static final String BLUE = "\u001B[34m";
  public static final String MAGENTA = "\u001B[35m";
  public static final String CYAN = "\u001B[36m";
  public static final String WHITE = "\u001B[37m";

  @Autowired
  private MaintainerService maintainerService;

  @Autowired
  private PackageService packageService;

  @Autowired
  private PackageMaintainerService packageMaintainerService;

  @Autowired
  private PackageVersionService packageVersionService;


  private BufferedReader stdin;
  private Map<String, Command> cmds;
  private String helpText;

  public Console() {
    stdin = new BufferedReader(new InputStreamReader(System.in));
    initCmds();
    initHelpText();
  }

  private void initCmds() {
    cmds = Map.ofEntries(
      entry("help", this::showHelp),
      entry("exit", this::exit),

      entry("add maintainer", this::addMaintainer),
      entry("delete maintainer", this::deleteMaintainer),
      entry("update maintainer", this::updateMaintainer),
      entry("show maintainers", this::showMaintainers),
      entry("filter maintainers", this::filterMaintainers),


      entry("add package", this::addPackage),
      entry("delete package", this::deletePackage),
      entry("update package", this::updatePackage),
      entry("show packages", this::showPackages),
      entry("filter packages", this::filterPackages),

      entry("add package-maintainer", this::addPackageMaintainer),
      entry("delete package-maintainer", this::deletePackageMaintainer),
      entry("update package-maintainer", this::updatePackageMaintainer),
      entry("show package-maintainers", this::showPackageMaintainers),
      entry("filter package-maintainers", this::filterPackageMaintainers),

      entry("add package-version", this::addPackageVersion),
      entry("delete package-version", this::deletePackageVersion),
      entry("update package-version", this::updatePackageVersion),
      entry("show package-versions", this::showPackageVersions),
      entry("filter package-versions", this::filterPackageVersions)

    );
  }


  private void initHelpText() {
    var sb = new StringBuilder();

    sb.append("                                          Available commands\n");
    sb.append("--------------------┳----------------------┳------------------------------┳---------------------------┓\n");
    sb.append("┃     Packages      ┃      Maintainers     ┃      PackageMaintainers      ┃      PackageVersions      ┃\n");
    sb.append("┣-------------------╋----------------------╋------------------------------╋---------------------------┫\n");
    sb.append("┃  add package      ┃  add maintainer      ┃  add package-maintainer      ┃  add package-version      ┃\n");
    sb.append("┃  update package   ┃  update maintainer   ┃  update package-maintainer   ┃  update package-version   ┃\n");
    sb.append("┃  delete package   ┃  delete maintainer   ┃  delete package-maintainer   ┃  delete package-version   ┃\n");
    sb.append("┃  show packages    ┃  show maintainers    ┃  show package-maintainers    ┃  show package-versions    ┃\n");
    sb.append("┃  filter packages  ┃  filter maintainers  ┃  filter package-maintainers  ┃  filter package-versions  ┃\n");
    sb.append("┣-------------------╋----------------------┻------------------------------┻---------------------------┛\n");
    sb.append("┃  help   ┃   exit  ┃\n");
    sb.append("┗---------┻---------┛\n");

    helpText =
      sb.toString()
        .replace("add", GREEN + "add")
        .replace("update", GREEN + "update")
        .replace("delete", GREEN + "delete")
        .replace("show", GREEN + "show")
        .replace("filter", GREEN + "filter")

        .replace("package", BLUE + "package")
        .replace("maintainer", BLUE + "maintainer")

        .replace("Package", YELLOW + "Package")
        .replace("Maintainer", YELLOW + "Maintainer")

        .replace("help", RED + "help")
        .replace("exit", RED + "exit")

        .replace("┃", RESET + "┃");

  }


  private String getLine() throws IOException {
    var line = stdin.readLine();

    if (line == null) {
      System.out.println("EoF detected. Quitting...");
      exit();
    }

    return line;
  }

  private String getCmd() throws IOException {
    var prompt = String.format(
      "\n%s╭─%s[%spackageManager%s]%s\n╰─%sλ%s ",
      GREEN, BLUE, YELLOW, BLUE, GREEN, RED, RESET);

    System.out.print(prompt);
    return getLine();
  }

  private String getString(String name) throws IOException {
    var prompt = String.format(
      "  %s~ %s%s%s: ",
      RED, GREEN, name, RESET);

    System.out.print(prompt);
    return getLine();
  }

  private Long getLong(String name) throws IOException {
    var prompt = String.format(
      "  %s# %s%s%s: ",
      RED, YELLOW, name, RESET);

    System.out.print(prompt);
    return Long.parseLong(getLine());
  }

  private void exit() {
    System.exit(0);
  }

  private void showHelp() {
    System.out.print(helpText);
  }

  private void addMaintainer() throws IOException {
    var userName = getString("userName");
    var fullName = getString("fullName");
    var email = getString("email");

    var maintainer = new Maintainer(userName, fullName, email);
    maintainerService.addMaintainer(maintainer);
  }

  private void deleteMaintainer() throws IOException {
    var id = getLong("id");
    maintainerService.deleteMaintainer(id);
  }

  private void updateMaintainer() throws IOException {
    var id = getLong("id");
    var userName = getString("userName");
    var fullName = getString("fullName");
    var email = getString("email");

    var maintainer = new Maintainer(userName, fullName, email);
    maintainer.setID(id);
    maintainerService.updateMaintainer(maintainer);
  }

  private void filterMaintainers() throws IOException {
    var type = getString("Field (userName | fullName | email)");
    var input = getString("Search term");

    maintainerService.getFilteredMaintainers(type, input).forEach(System.out::println);
  }

  private void showMaintainers() throws IOException {
    maintainerService.getAllMaintainers().forEach(System.out::println);
  }


  private void addPackage() throws IOException {
    var name = getString("name");
    var description = getString("description");
    var sourceRepo = getString("sourceRepo");
    var license = getString("license");

    var pkg = new Package(name, description, sourceRepo, license);
    packageService.addPackage(pkg);
  }

  private void deletePackage() throws IOException {
    var id = getLong("id");
    packageService.deletePackage(id);
  }

  private void updatePackage() throws IOException {
    var id = getLong("id");
    var name = getString("name");
    var description = getString("description");
    var sourceRepo = getString("sourceRepo");
    var license = getString("license");

    var pkg = new Package( name, description, sourceRepo, license);
    packageService.updatePackage(pkg);
  }

  private void filterPackages() throws IOException {
    var type = getString("Field (name | description | sourceRepo | license)");
    var input = getString("Search term");

    packageService.getFilteredPackages(type, input).forEach(System.out::println);
  }

  private void showPackages() throws IOException {
    packageService.getAllPackages().forEach(System.out::println);
  }

  private void addPackageMaintainer() throws IOException {
    var maintainerID = getLong("maintainerID");
    var packageID = getLong("packageID");

    var packageMaintainer = new PackageMaintainer(maintainerID, packageID);
    packageMaintainerService.addPackageMaintainer(packageMaintainer);
  }

  private void deletePackageMaintainer() throws IOException {
    var id = getLong("id");
    packageMaintainerService.deletePackageMaintainer(id);
  }

  private void updatePackageMaintainer() throws IOException {
    var id = getLong("id");
    var maintainerID = getLong("maintainerID");
    var packageID = getLong("packageID");

    var packageMaintainer = new PackageMaintainer(maintainerID, packageID);
    packageMaintainer.setID(id);

    packageMaintainerService.updatePackageMaintainer(packageMaintainer);
  }

  private void filterPackageMaintainers() throws IOException {
    var field = getString("Field (maintainerID | packageID)");
    var searchTerm = getString("Search term");

    packageMaintainerService.getFilteredPackageMaintainers(field, searchTerm).forEach(System.out::println);
  }

  private void showPackageMaintainers() throws IOException {
    packageMaintainerService.getAllPackageMaintainers().forEach(System.out::println);
  }

  private void addPackageVersion() throws IOException {
    var packageID = getLong("packageID");
    var versionNumber = getString("versionNumber");

    var packageVersion = new PackageVersion( packageID, versionNumber);
    packageVersionService.addPackageVersion(packageVersion);
  }

  private void updatePackageVersion() throws IOException {
    var id = getLong("id");
    var packageID = getLong("packageID");
    var versionNumber = getString("versionNumber");

    var packageVersion = new PackageVersion( packageID, versionNumber);
    packageVersionService.addPackageVersion(packageVersion);
  }

  private void deletePackageVersion() throws IOException {
    var id = getLong("id");
    packageVersionService.deletePackageVersion(id);
  }

  private void showPackageVersions() {
    packageVersionService.getAllPackageVersions().forEach(System.out::println);
  }

  private void filterPackageVersions() throws IOException {
    var type = getString("Field (packageID | versionNumber)");
    var input = getString("Search term");

    packageVersionService.getFilteredPackageVersions(type, input).forEach(System.out::println);
  }


  public void run() {
    showHelp();

    while (true) {
      try {
        var cmd = getCmd();

        if (cmds.containsKey(cmd))
          cmds.get(cmd).run();
        else if (!cmd.isBlank())
          System.out.println("Command not recognized.");


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
