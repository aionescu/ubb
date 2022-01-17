package ro.arc.packageManager.core.service;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ro.arc.packageManager.core.repository.PackageMaintainerRepository;
import ro.arc.packageManager.core.domain.PackageMaintainer;
import ro.arc.packageManager.core.domain.exceptions.AppException;
import ro.arc.packageManager.core.domain.exceptions.Contract;
import ro.arc.packageManager.core.domain.validators.PackageMaintainerValidator;


import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

@Service
public class PackageMaintainerServiceImpl implements PackageMaintainerService {
    public static final Logger log = LoggerFactory.getLogger(PackageMaintainerServiceImpl.class);

    @Autowired
    private PackageMaintainerRepository packageMaintainerRepo;

    @Autowired
    private PackageMaintainerValidator validator;

    @Override
    @Transactional
    public PackageMaintainer addPackageMaintainer(PackageMaintainer packageMaintainer) {
        log.trace("addPackageMaintainer - method entered: packageMaintainer={}", packageMaintainer);
        validator.validate(packageMaintainer);

        var existsPackageMaintainer = packageMaintainerRepo.existsAllByMaintainerIDAndPackageID(
          packageMaintainer.getMaintainerID(),
          packageMaintainer.getPackageID()
        );

        Contract.ensure(!existsPackageMaintainer, "That person is already a maintainer for the specified package.");

        var result = this.packageMaintainerRepo.save(packageMaintainer);
        log.trace("addPackageMaintainer - result={}", result);
        return result;
    }

    @Override
    @Transactional
    public PackageMaintainer updatePackageMaintainer(PackageMaintainer packageMaintainer)  {
        log.trace("updatePackageMaintainer - method entered: packageMaintainer={}", packageMaintainer);
        var result = this.packageMaintainerRepo.findById(packageMaintainer.getId()).orElseThrow(() -> new AppException("Non-existent package version with this ID !"));

        var existsPackageMaintainer = packageMaintainerRepo.existsAllByMaintainerIDAndPackageID(
          packageMaintainer.getMaintainerID(),
          packageMaintainer.getPackageID()
        );

        Contract.ensure(!existsPackageMaintainer, "That person is already a maintainer for the specified package.");

        result.setMaintainerID(packageMaintainer.getMaintainerID());
        result.setPackageID(packageMaintainer.getPackageID());
        log.trace("updatePackageMaintainer - result={}", result);
        return result;
    }

    @Override
    public void deletePackageMaintainer(Long ID){
        log.trace("deletePackageMaintainer - method entered : id={}", ID);
        this.packageMaintainerRepo.findById(ID).orElseThrow(() -> new AppException("Non-existent package version with this ID !"));
        this.packageMaintainerRepo.deleteById(ID);
        log.trace("deletePackageMaintainer - method finished");
    }

    @Override
    public List<PackageMaintainer> getFilteredPackageMaintainers(String type, String input) {
        log.trace("filteredPackageMaintainers - method entered : type={}, input={}",type, input);
        List<PackageMaintainer> result;
        var packageMaintainers = this.packageMaintainerRepo.findAll();
        switch (type) {
            case "packageID":
                result = packageMaintainerRepo.findAllByPackageID(Long.parseLong(input));
                log.trace("filteredPackageMaintainers - result={}", result);
                return result;
            case "maintainerID":
                result = packageMaintainerRepo.findAllByMaintainerID(Long.parseLong(input));
                log.trace("filteredPackageMaintainers - result={}", result);
                return result;
        }
        throw new AppException("This type does not exist !");
    }

    @Override
    public List<PackageMaintainer> getAllPackageMaintainers() {
        log.trace("allPackageMaintainers - method entered");
        var packageMaintainers = this.packageMaintainerRepo.findAll();
        log.trace("allPackageMaintainers - result={}", packageMaintainers);
        return new ArrayList<>(packageMaintainers);
    }
}