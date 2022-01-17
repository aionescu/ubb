package ro.arc.packageManager.client.ui;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.RestTemplate;
import ro.arc.packageManager.core.domain.PackageVersion;
import ro.arc.packageManager.core.domain.exceptions.AppException;
import ro.arc.packageManager.web.dto.*;

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

  private static final String maintainersUrl = "http://localhost:8080/api/maintainers";
  private static final String packagesUrl = "http://localhost:8080/api/packages";
  private static final String packageVersionsUrl = "http://localhost:8080/api/packageVersions";
  private static final String packageMaintainersUrl = "http://localhost:8080/api/packageMaintainers";

  @Autowired
  private RestTemplate restTemplate;

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

    var maintainerDto =new MaintainerDto(userName, fullName, email);
    try {
      restTemplate.postForObject(maintainersUrl, maintainerDto, MaintainerDto.class);
      System.out.println("Maintainer added successfully");
    }
    catch (RestClientException e)
    {
      System.out.println("Maintainer couldn't be added");
    }
  }

  private void deleteMaintainer() throws IOException {
    var id = getLong("id");
    try {
      restTemplate.delete(maintainersUrl + "/{id}", id);
      System.out.println("Maintainer deleted successfully");
    }
    catch (RestClientException e)
    {
      System.out.println("Maintainer couldn't be deleted");
    }
  }

  private void updateMaintainer() throws IOException {
    var id = getLong("id");
    var userName = getString("userName");
    var fullName = getString("fullName");
    var email = getString("email");

    var maintainerDto =new MaintainerDto(userName, fullName, email);
    maintainerDto.setId(id);
    try {
      restTemplate.put(maintainersUrl + "/{id}", maintainerDto, maintainerDto.getId());
      System.out.println("Maintainer updated successfully");
    }
    catch (RestClientException e)
    {
      System.out.println("Maintainer couldn't be updated");
    }
  }

  private void filterMaintainers() throws IOException {
    var type = getString("Field (userName | fullName | email)");
    var input = getString("Search term");

    try {
      MaintainersDto maintainers = restTemplate.getForObject(maintainersUrl + "/filter/{type}_{input}", MaintainersDto.class, type, input);
      if (maintainers != null)
        maintainers.getMaintainers().forEach(System.out::println);
    }
    catch (RestClientException e)
    {
      System.out.println("Incorrect type");
    }
  }

  private void showMaintainers() throws IOException {
    MaintainersDto maintainers = restTemplate.getForObject(maintainersUrl, MaintainersDto.class);
    try {
      if (maintainers != null)
        maintainers.getMaintainers().forEach(System.out::println);
    }
    catch (RestClientException e)
    {
      System.out.println("Error occurred");
    }
  }

  private void addPackage() throws IOException {

    var name = getString("name");
    var description = getString("description");
    var sourceRepo = getString("sourceRepo");
    var license = getString("license");

    var packageDto = new PackageDto(name,description,sourceRepo,license);

    try {
      restTemplate.postForObject(packagesUrl , packageDto, PackageDto.class);
      System.out.println("Package added successfully");
    }catch (RestClientException e)
    {
      System.out.println("Package couldn't be added");
    }
  }

  private void deletePackage() throws IOException {

    var id = getLong("id");
    try {
      restTemplate.delete(packagesUrl + "/{id}", id);
      System.out.println("Package deleted successfully");
    }
    catch (RestClientException e)
    {
      System.out.println("Package couldn't be deleted");
    }
  }

  private void updatePackage() throws IOException {
    var id = getLong("id");
    var name = getString("name");
    var description = getString("description");
    var sourceRepo = getString("sourceRepo");
    var license = getString("license");

    var packageDto =new PackageDto(name,description,sourceRepo,license);
    packageDto.setId(id);
    try {
      restTemplate.put(packagesUrl + "/{id}", packageDto, packageDto.getId());
      System.out.println("Package updated successfully");
    }
    catch (RestClientException e)
    {
      System.out.println("package couldn't be updated");
    }
  }

  private void filterPackages() throws IOException {
    var type = getString("Field (name | description | sourceRepo | license)");
    var input = getString("Search term");


    try {
      PackagesDto packages = restTemplate.getForObject(packagesUrl + "/filter/{type}_{input}", PackagesDto.class, type, input);
      if (packages != null)
        packages.getPackages().forEach(System.out::println);
    }
    catch (RestClientException e)
    {
      System.out.println("Incorrect type");
    }
  }

  private void showPackages() throws IOException {
    PackagesDto packages = restTemplate.getForObject(packagesUrl, PackagesDto.class);
    try {
      if (packages != null)
        packages.getPackages().forEach(System.out::println);
    }
    catch (RestClientException e)
    {
      System.out.println("Error occurred");
    }
  }

  private void addPackageMaintainer() throws IOException {
    var maintainerID = getLong("maintainerID");
    var packageID = getLong("packageID");

    var packageMaintainerDto = new PackageMaintainerDto(maintainerID, packageID);

    try {
      restTemplate.postForObject(packageMaintainersUrl, packageMaintainerDto, PackageMaintainerDto.class);
      System.out.println("PackageMaintainer added successfully");
    }catch (RestClientException e)
    {
      System.out.println("PackageMaintainer couldn't be added");
    }
  }

  private void updatePackageMaintainer() throws IOException {
    var id = getLong("id");
    var maintainerID = getLong("maintainerID");
    var packageID = getLong("packageID");

    var packageMaintainerDto = new PackageMaintainerDto(maintainerID, packageID);
    packageMaintainerDto.setId(id);

    try {
      restTemplate.put(packageMaintainersUrl + "/{id}", packageMaintainerDto, packageMaintainerDto.getId());
      System.out.println("PackageMaintainer updated successfully");
    }
    catch (RestClientException e)
    {
      System.out.println("PackageMaintainer couldn't be updated");
    }
  }

  private void deletePackageMaintainer() throws IOException {
    var id = getLong("id");

    try {
      restTemplate.delete(packageMaintainersUrl + "/{id}", id);
      System.out.println("PackageMaintainer deleted successfully");
    } catch (RestClientException e) {
      System.out.println("PackageMaintainer couldn't be deleted");
    }
  }

  private void showPackageMaintainers() {
    var packageMaintainers = restTemplate.getForObject(packageMaintainersUrl, PackageMaintainersDto.class);

    try {
      if (packageMaintainers != null)
        packageMaintainers.getPackageMaintainers().forEach(System.out::println);
    } catch (RestClientException e) {
      System.out.println("Error occurred");
    }
  }

  private void filterPackageMaintainers() throws IOException {
    var type = getString("Field (maintainerID | packageID)");
    var input = getString("Search term");

    try {
      var packageMaintainers = restTemplate.getForObject(packageMaintainersUrl + "/filter/{type}_{input}", PackageMaintainersDto.class, type, input);

      if (packageMaintainers != null)
        packageMaintainers.getPackageMaintainers().forEach(System.out::println);
    } catch (RestClientException e) {
      System.out.println("Incorrect type");
    }
  }

  private void addPackageVersion() throws IOException {
    var packageID = getLong("packageID");
    var versionNumber = getString("versionNumber");

    var packageVersionDto = new PackageVersionDto(packageID, versionNumber);

    try {
      restTemplate.postForObject(packageVersionsUrl , packageVersionDto, PackageVersionDto.class);
      System.out.println("Package version added successfully");
    }catch (RestClientException e)
    {
      System.out.println("Package version couldn't be added");
    }
  }

  private void updatePackageVersion() throws IOException {
    var id = getLong("id");
    var packageID = getLong("packageID");
    var versionNumber = getString("versionNumber");

    var packageVersionDto =new PackageVersionDto(packageID,versionNumber);
    packageVersionDto.setId(id);
    try {
      restTemplate.put(packageVersionsUrl + "/{id}", packageVersionDto, packageVersionDto.getId());
      System.out.println("Package version updated successfully");
    }
    catch (RestClientException e)
    {
      System.out.println("Package version couldn't be updated");
    }
  }

  private void deletePackageVersion() throws IOException {
    var id = getLong("id");
    try {
      restTemplate.delete(packageVersionsUrl + "/{id}", id);
      System.out.println("Package version deleted successfully");
    }
    catch (RestClientException e)
    {
      System.out.println("Package version couldn't be deleted");
    }
  }

  private void showPackageVersions() {
    PackageVersionsDto packageVersions = restTemplate.getForObject(packageVersionsUrl, PackageVersionsDto.class);
    try {
      if (packageVersions != null)
        packageVersions.getPackageVersions().forEach(System.out::println);
    }
    catch (RestClientException e)
    {
      System.out.println("Error occurred");
    }
  }

  private void filterPackageVersions() throws IOException {
    var type = getString("Field (packageID | versionNumber)");
    var input = getString("Search term");

    try {
      PackageVersionsDto packageVersions = restTemplate.getForObject(packageVersionsUrl + "/filter/{type}_{input}", PackageVersionsDto.class, type, input);
      if (packageVersions != null)
        packageVersions.getPackageVersions().forEach(System.out::println);
    }
    catch (RestClientException e)
    {
      System.out.println("Incorrect type");
    }
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
