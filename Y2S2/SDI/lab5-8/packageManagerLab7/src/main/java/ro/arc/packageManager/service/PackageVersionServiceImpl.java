package ro.arc.packageManager.service;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import ro.arc.packageManager.domain.Package;
import ro.arc.packageManager.domain.PackageVersion;
import ro.arc.packageManager.domain.exceptions.AppException;
import ro.arc.packageManager.domain.exceptions.Contract;
import ro.arc.packageManager.domain.validators.PackageValidator;
import ro.arc.packageManager.domain.validators.PackageVersionValidator;
import ro.arc.packageManager.repository.PackageRepository;
import ro.arc.packageManager.repository.PackageVersionRepository;

import javax.transaction.Transactional;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

@Service
public class PackageVersionServiceImpl implements PackageVersionService{
    public static final Logger log = LoggerFactory.getLogger(PackageVersionServiceImpl.class);

    @Autowired
    private PackageVersionRepository packageVersionRepo;

    @Autowired
    private PackageVersionValidator validator;

    @Override
    @Transactional
    public void addPackageVersion(PackageVersion packageVersion) {
        log.trace("addPackageVersion - method entered: packageVersion={}", packageVersion);
        validator.validate(packageVersion);
        Contract.ensure(!existsPackageVersion(packageVersion.getPackageID()), "The package with this ID already has a package version.");

        this.packageVersionRepo.save(packageVersion);
        log.trace("addPackageVersion - method finished");
    }
    private boolean existsPackageVersion(Long packageID) {
        Contract.notNull(packageID, "packageID");

        var all = this.packageVersionRepo.findAll();

        return
                StreamSupport.stream(all.spliterator(), false)
                        .anyMatch(pv -> pv.getPackageID().equals(packageID));
    }

    @Override
    @Transactional
    public void updatePackageVersion(PackageVersion packageVersion)  {
        log.trace("updatePackageVersion - method entered: packageVersion={}", packageVersion);
        this.packageVersionRepo.findById(packageVersion.getID()).orElseThrow(() -> new AppException("Non-existent package version with this ID !"));
        Contract.ensure(!existsPackageVersion(packageVersion.getPackageID()), "The package with this ID already has a package version.");

        this.packageVersionRepo.findById(packageVersion.getID()).
                ifPresent(assig -> {
                            assig.setPackageID(packageVersion.getPackageID());
                            assig.setVersionNumber(packageVersion.getVersionNumber());
                           }
                );
        log.trace("updatePackageVersion - method finished");
    }

    @Override
    public void deletePackageVersion(Long ID){
        log.trace("deletePackageVersion - method entered : id={}", ID);
        this.packageVersionRepo.findById(ID).orElseThrow(() -> new AppException("Non-existent package version with this ID !"));
        this.packageVersionRepo.deleteById(ID);
        log.trace("deletePackageVersion - method finished");
    }

    @Override
    public List<PackageVersion> getFilteredPackageVersions(String type, String input) {
        log.trace("filteredPackageVersions - method entered : type={}, input={}",type, input);
        List<PackageVersion> result;
        var packageVersions = this.packageVersionRepo.findAll();
        switch (type) {
            case "packageID":
                result =  StreamSupport.stream(packageVersions.spliterator(), false).filter(p -> p.getPackageID().toString().contains(input)).collect(Collectors.toList());
                log.trace("filteredPackageVersions - result={}", result);
                return result;
            case "versionNumber":
                result =  StreamSupport.stream(packageVersions.spliterator(), false).filter(p -> p.getVersionNumber().contains(input)).collect(Collectors.toList());
                log.trace("filteredPackageVersions - result={}", result);
                return result;
        }
        throw new AppException("This type does not exist !");
    }

    @Override
    public List<PackageVersion> getAllPackageVersions() {
        log.trace("allPackageVersions - method entered");
        var packageVersions = this.packageVersionRepo.findAll();
        log.trace("allPackageVersions - result={}", packageVersions);
        return new ArrayList<>(packageVersions);
    }
}