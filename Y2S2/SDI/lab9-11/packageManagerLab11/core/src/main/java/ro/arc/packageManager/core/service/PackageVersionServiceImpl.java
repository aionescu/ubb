package ro.arc.packageManager.core.service;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ro.arc.packageManager.core.domain.Package;
import ro.arc.packageManager.core.domain.PackageVersion;
import ro.arc.packageManager.core.domain.exceptions.AppException;
import ro.arc.packageManager.core.domain.exceptions.Contract;
import ro.arc.packageManager.core.domain.validators.PackageVersionValidator;
import ro.arc.packageManager.core.repository.PackageRepository;
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
    private PackageRepository packageRepository;

    @Autowired
    private PackageVersionValidator validator;

    @Override
    @Transactional
    public PackageVersion addPackageVersion(PackageVersion packageVersion) {
        log.trace("addPackageVersion - method entered: packageVersion={}", packageVersion);
        System.out.println("service " + packageVersion);
        validator.validate(packageVersion);

        System.out.println("check 1");
        var versionNumber = packageVersionRepo.existsAllByVersionNumber(packageVersion.getVersionNumber());
        Contract.ensure(!versionNumber, "A package version with that version number already exists.");

        System.out.println("check 2");
        var resultPackage = this.packageRepository.findById(packageVersion.getAPackage().getId()).orElseThrow(() -> new AppException("Non existent package with this ID !"));

        System.out.println("check 3");
        try {
            var result = this.packageVersionRepo.save(packageVersion);
            System.out.println("check 4");
            log.trace("addPackageVersion - result={}", result);
            return result;
        } catch (Exception e) {
            System.out.println(e.getMessage());
        }
        return null;
    }

    @Override
    @Transactional
    public PackageVersion updatePackageVersion(PackageVersion packageVersion)  {
        log.trace("updatePackageVersion - method entered: packageVersion={}", packageVersion);
        validator.validate(packageVersion);
        var result = this.packageVersionRepo.findById(packageVersion.getId()).orElseThrow(() -> new AppException("Non-existent package version with this ID !"));

        var resultPackage = this.packageRepository.findById(packageVersion.getAPackage().getId()).orElseThrow(() -> new AppException("Non existent package with this ID !"));
        result.setAPackage(resultPackage);
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
    public List<PackageVersion> getFilteredPackageVersions(Package aPackage) {
        log.trace("filteredPackageVersions - method entered : package={}",aPackage);
        List<PackageVersion> result;
        var packageVersions = packageVersionRepo.findAllByAPackage(aPackage);
        return new ArrayList<>(packageVersions);
    }

    @Override
    public List<PackageVersion> getAllPackageVersions() {
        log.trace("allPackageVersions - method entered");
        var packageVersions = this.packageVersionRepo.findAll();
        log.trace("allPackageVersions - result={}", packageVersions);
        return new ArrayList<>(packageVersions);
    }
}