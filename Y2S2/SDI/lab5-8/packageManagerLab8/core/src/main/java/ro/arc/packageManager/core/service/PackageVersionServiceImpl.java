package ro.arc.packageManager.core.service;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ro.arc.packageManager.core.domain.PackageVersion;
import ro.arc.packageManager.core.domain.exceptions.AppException;
import ro.arc.packageManager.core.domain.exceptions.Contract;
import ro.arc.packageManager.core.domain.validators.PackageVersionValidator;
import ro.arc.packageManager.core.repository.PackageVersionRepository;

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
    public PackageVersion addPackageVersion(PackageVersion packageVersion) {
        log.trace("addPackageVersion - method entered: packageVersion={}", packageVersion);
        validator.validate(packageVersion);
        var existentPackageVersion = packageVersionRepo.existsAllByPackageID(packageVersion.getPackageID());
        Contract.ensure(!existentPackageVersion, "The package with this ID already has a package version.");

        var result = this.packageVersionRepo.save(packageVersion);
        log.trace("addPackageVersion - result={}", result);
        return result;
    }

    @Override
    @Transactional
    public PackageVersion updatePackageVersion(PackageVersion packageVersion)  {
        log.trace("updatePackageVersion - method entered: packageVersion={}", packageVersion);
        var result = this.packageVersionRepo.findById(packageVersion.getId()).orElseThrow(() -> new AppException("Non-existent package version with this ID !"));
        var existentPackageVersion = packageVersionRepo.existsAllByPackageID(packageVersion.getPackageID());
        Contract.ensure(!existentPackageVersion, "The package with this ID already has a package version.");

        result.setPackageID(packageVersion.getPackageID());
        result.setVersionNumber(packageVersion.getVersionNumber());
        log.trace("updatePackageVersion - result={}", result);
        return result;
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
                result = packageVersionRepo.findAllByPackageID(Long.parseLong(input));
                log.trace("filteredPackageVersions - result={}", result);
                return result;
            case "versionNumber":
                result = packageVersionRepo.findAllByVersionNumberContains(input);
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