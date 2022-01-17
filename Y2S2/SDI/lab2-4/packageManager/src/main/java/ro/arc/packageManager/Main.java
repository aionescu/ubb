package ro.arc.packageManager;

import org.postgresql.shaded.com.ongres.scram.common.bouncycastle.pbkdf2.Pack;
import ro.arc.packageManager.domain.*;
import ro.arc.packageManager.domain.FormatXML;
import ro.arc.packageManager.domain.Maintainer;
import ro.arc.packageManager.domain.Package;
import ro.arc.packageManager.domain.PackageMaintainer;
import ro.arc.packageManager.domain.validators.MaintainerValidator;
import ro.arc.packageManager.domain.validators.PackageMaintainerValidator;
import ro.arc.packageManager.domain.validators.PackageValidator;
import ro.arc.packageManager.repository.InMemoryRepository;
import ro.arc.packageManager.repository.Repository;
import ro.arc.packageManager.repository.XMLRepository;
import ro.arc.packageManager.repository.DBRepository;
import ro.arc.packageManager.repository.FileRepository;
import ro.arc.packageManager.service.MaintainerService;
import ro.arc.packageManager.service.PackageService;
import ro.arc.packageManager.service.PackageMaintainerService;
import ro.arc.packageManager.ui.ConsoleUI;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.Random;
import java.util.Scanner;

import static java.lang.Integer.parseInt;
import static java.lang.Integer.toUnsignedLong;


public class Main {
  public static void main(String args[]) {

    var maintainerValidator = new MaintainerValidator();
    var packageValidator = new PackageValidator();
    var packageMaintainerValidator = new PackageMaintainerValidator();


    Repository<Long, Maintainer> maintainerRepo = null;
    Repository<Long, Package> packageRepo = null;
    Repository<Long, PackageMaintainer> packageMaintainerRepo = null;
    boolean validChoice = false;
    while(!validChoice) {
      System.out.println("Choose repo congiguration:\n \t╰─1.InMemory\n\t╰─2.File\n\t╰─3.Database\n\t╰─4.XML ");
      Scanner in = new Scanner(System.in);
      int repoType = parseInt(in.nextLine());
      switch (repoType) {
        case 1:

          maintainerRepo = new InMemoryRepository<>(maintainerValidator);
          packageRepo = new InMemoryRepository<>(packageValidator);
          packageMaintainerRepo = new InMemoryRepository<>(packageMaintainerValidator);

          for(int i = 0 ;i<100 ; i++)
          {
            Maintainer m = new Maintainer(toUnsignedLong(i), "user"+i, "name"+i, "email"+i+"@yahoo.com");
            Package p = new Package(toUnsignedLong(i), "name"+i, "description"+i, "https://source"+i+".com", "license"+i);
            maintainerRepo.save(m);
            packageRepo.save(p);
          }
          var rand = new Random();
          for(int i=0; i<100 ;i++)
          {
            PackageMaintainer pkgMaintainer = new PackageMaintainer(toUnsignedLong(i) , toUnsignedLong(rand.nextInt(100)),toUnsignedLong(rand.nextInt(100)));
            packageMaintainerRepo.save(pkgMaintainer);
          }

          validChoice = true;
          break;

        case 2:
          validChoice = true;

          maintainerRepo = new FileRepository<>("packageManager/data/csv/maintainers.csv", maintainerValidator, Maintainer.class);
          packageRepo = new FileRepository<>("packageManager/data/csv/packages.csv", packageValidator, Package.class);

          packageMaintainerRepo =
            new FileRepository<>(
              "packageManager/data/csv/packageMaintainers.csv",
              packageMaintainerValidator,
              PackageMaintainer.class);
          break;

        case 3:
          String url = "jdbc:postgresql://localhost:5432/packageMaintainerJDBC";
          String user = "postgres";
          String password = "password";
          var maintainerHelper = new MaintainerHelper();
          var packageHelper = new PackageHelper();
          var packageMaintainerHelper = new PackageMaintainerHelper();
          maintainerRepo = new DBRepository<Maintainer>(maintainerValidator, maintainerHelper, url, user, password);
          packageRepo = new DBRepository<Package>(packageValidator, packageHelper, url, user, password);
          packageMaintainerRepo = new DBRepository<PackageMaintainer>(packageMaintainerValidator, packageMaintainerHelper, url, user, password);
          validChoice = true;
          break;

        case 4:
          Maintainer.MaintainerFormat maintainerFormat = new Maintainer.MaintainerFormat();
          Package.PackageFormat packageFormat = new Package.PackageFormat();
          PackageMaintainer.PackageMaintainerFormat packageMaintainerFormat = new PackageMaintainer.PackageMaintainerFormat();
          maintainerRepo = new XMLRepository<>(maintainerValidator, "packageManager/data/maintainer.xml", maintainerFormat);
          packageRepo = new XMLRepository<>(packageValidator, "packageManager/data/package.xml", packageFormat);
          packageMaintainerRepo = new XMLRepository<>(packageMaintainerValidator, "packageManager/data/packageMaintainer.xml", packageMaintainerFormat);
          validChoice = true;
          break;

        default:
          System.out.println("Invalid choice! Please try again!");
      }
    }
    var serviceMaintainer = new MaintainerService(maintainerRepo);
    var servicePackage = new PackageService(packageRepo);
    var servicePackageMaintainer = new PackageMaintainerService(maintainerRepo, packageRepo, packageMaintainerRepo);



    var ui = new ConsoleUI(serviceMaintainer, servicePackage, servicePackageMaintainer);
    ui.run();
  }
}
