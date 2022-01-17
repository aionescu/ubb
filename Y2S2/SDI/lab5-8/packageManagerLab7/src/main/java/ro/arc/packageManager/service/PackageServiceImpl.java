package ro.arc.packageManager.service;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import ro.arc.packageManager.domain.Package;
import ro.arc.packageManager.domain.exceptions.AppException;
import ro.arc.packageManager.domain.exceptions.Contract;
import ro.arc.packageManager.domain.validators.PackageValidator;
import ro.arc.packageManager.repository.PackageRepository;

import javax.transaction.Transactional;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

@Service
public class PackageServiceImpl implements PackageService{
    public static final Logger log = LoggerFactory.getLogger(PackageServiceImpl.class);

    @Autowired
    private PackageRepository packageRepo;

    @Autowired
    private PackageValidator validator;

    @Override
    @Transactional
    public void addPackage(Package pkg) {
        log.trace("addPackage - method entered: package={}", pkg);
        validator.validate(pkg);
        Contract.ensure(!existsPackageName(pkg.getName()), "A package with that name already exists.");

        this.packageRepo.save(pkg);
        log.trace("addPackage - method finished");
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
    @Transactional
    public void updatePackage(Package pkg)  {
        log.trace("updatePackage - method entered: package={}", pkg);
        this.packageRepo.findById(pkg.getID()).orElseThrow(() -> new AppException("Non-existent package with this ID !"));
        Contract.ensure(!existPackageNameDifferentID(pkg.getName(), pkg.getID()), "A package with the new name already exists.");

        this.packageRepo.findById(pkg.getID()).
                ifPresent(assig -> {
                            assig.setName(pkg.getName());
                            assig.setDescription(pkg.getDescription());
                            assig.setLicense(pkg.getLicense());
                            assig.setSourceRepo(pkg.getSourceRepo());
                        }
                );
        log.trace("updatePackage - method finished");
    }

    @Override
    public void deletePackage(Long ID){
        log.trace("deletePackage - method entered : id={}", ID);
        this.packageRepo.findById(ID).orElseThrow(() -> new AppException("Non-existent package with this ID !"));
        this.packageRepo.deleteById(ID);
        log.trace("deletePackage - method finished");
    }

    @Override
    public List<Package> getFilteredPackages(String type, String input) {
        log.trace("filteredPackages - method entered : type={}, input={}",type, input);
        List<Package> result;
        var packages = this.packageRepo.findAll();
        switch (type) {
            case "name":
                result =  StreamSupport.stream(packages.spliterator(), false).filter(p -> p.getName().contains(input)).collect(Collectors.toList());
                log.trace("filteredPackages - result={}", result);
                return result;
            case "description":
                result =  StreamSupport.stream(packages.spliterator(), false).filter(p -> p.getDescription().contains(input)).collect(Collectors.toList());
                log.trace("filteredPackages - result={}", result);
                return result;
            case "license":
                result =  StreamSupport.stream(packages.spliterator(), false).filter(p -> p.getLicense().contains(input)).collect(Collectors.toList());
                log.trace("filteredPackages - result={}", result);
                return result;
            case "sourceRepo":
                result =  StreamSupport.stream(packages.spliterator(), false).filter(p -> p.getSourceRepo().contains(input)).collect(Collectors.toList());
                log.trace("filteredPackages - result={}", result);
                return result;
        }
        throw new AppException("This type does not exist !");
    }

    @Override
    public List<Package> getAllPackages() {
        log.trace("allPackages - method entered");
        var packages = this.packageRepo.findAll();
        log.trace("allPackages - result={}", packages);
        return new ArrayList<>(packages);
    }
}
