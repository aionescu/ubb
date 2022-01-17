package ro.arc.packageManager.core.service;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ro.arc.packageManager.core.domain.Package;
import ro.arc.packageManager.core.domain.exceptions.AppException;
import ro.arc.packageManager.core.domain.exceptions.Contract;
import ro.arc.packageManager.core.domain.validators.PackageValidator;
import ro.arc.packageManager.core.repository.PackageRepository;


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
    public Package addPackage(Package pkg) {
        log.trace("addPackage - method entered: package={}", pkg);
        validator.validate(pkg);
        var name = packageRepo.existsAllByName(pkg.getName());
        Contract.ensure(!name, "A package with that name already exists.");

        var result = this.packageRepo.save(pkg);
        log.trace("addPackage - result={}", result);
        return result;
    }

    @Override
    public Package addPackageAnyCase(Package pkg) {
        return this.packageRepo.save(pkg);
    }


    @Override
    @Transactional
    public Package updatePackage(Package pkg)  {
        log.trace("updatePackage - method entered: package={}", pkg);
        validator.validate(pkg);
        var result = this.packageRepo.findById(pkg.getId()).orElseThrow(() -> new AppException("Non-existent package with this ID !"));
        var sameNameDifferentId = packageRepo.existsAllByNameAndIdIsNot(pkg.getName(), pkg.getId());
        Contract.ensure(!sameNameDifferentId, "A package with the new name already exists.");

        result.setName(pkg.getName());
        result.setDescription(pkg.getDescription());
        result.setSourceRepo(pkg.getSourceRepo());
        result.setLicense(pkg.getLicense());
        log.trace("updatePackage - result={}", result);
        return result;
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
                result = packageRepo.findAllByNameContains(input);
                log.trace("filteredPackages - result={}", result);
                return result;
            case "description":
                result = packageRepo.findAllByDescriptionContains(input);
                log.trace("filteredPackages - result={}", result);
                return result;
            case "license":
                result = packageRepo.findAllByLicenseContains(input);
                log.trace("filteredPackages - result={}", result);
                return result;
            case "sourceRepo":
                result = packageRepo.findAllBySourceRepoContains(input);
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

    @Override
    public Package getOnePackage(Long ID) {
        return this.packageRepo.findById(ID).orElseThrow(() -> new AppException("Non-existent package with this ID !"));
    }

    @Override
    public Package getPackageByName(String name) {
        return this.packageRepo.findByName(name).orElseThrow(() -> new AppException("Non-existent package with this name !"));
    }
}
