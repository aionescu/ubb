package ro.arc.packageManager.service;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import ro.arc.packageManager.domain.Package;
import ro.arc.packageManager.domain.PackageMaintainer;
import ro.arc.packageManager.domain.exceptions.AppException;
import ro.arc.packageManager.domain.exceptions.Contract;
import ro.arc.packageManager.domain.validators.PackageValidator;
import ro.arc.packageManager.domain.validators.PackageMaintainerValidator;
import ro.arc.packageManager.repository.PackageRepository;
import ro.arc.packageManager.repository.PackageMaintainerRepository;

import javax.transaction.Transactional;
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
    public void addPackageMaintainer(PackageMaintainer packageMaintainer) {
        log.trace("addPackageMaintainer - method entered: packageMaintainer={}", packageMaintainer);
        validator.validate(packageMaintainer);
        Contract.ensure(!existsPackageMaintainer(packageMaintainer.getPackageID()), "The package with this ID already has a package version.");

        this.packageMaintainerRepo.save(packageMaintainer);
        log.trace("addPackageMaintainer - method finished");
    }
    private boolean existsPackageMaintainer(Long packageID) {
        Contract.notNull(packageID, "packageID");

        var all = this.packageMaintainerRepo.findAll();

        return
                StreamSupport.stream(all.spliterator(), false)
                        .anyMatch(pv -> pv.getPackageID().equals(packageID));
    }

    @Override
    @Transactional
    public void updatePackageMaintainer(PackageMaintainer packageMaintainer)  {
        log.trace("updatePackageMaintainer - method entered: packageMaintainer={}", packageMaintainer);
        this.packageMaintainerRepo.findById(packageMaintainer.getID()).orElseThrow(() -> new AppException("Non-existent package version with this ID !"));
        Contract.ensure(!existsPackageMaintainer(packageMaintainer.getPackageID()), "The package with this ID already has a package version.");

        this.packageMaintainerRepo.findById(packageMaintainer.getID()).
                ifPresent(assig -> {
                            assig.setMaintainerID(packageMaintainer.getMaintainerID());
                            assig.setPackageID(packageMaintainer.getPackageID());
                           }
                );
        log.trace("updatePackageMaintainer - method finished");
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
                result =  StreamSupport.stream(packageMaintainers.spliterator(), false).filter(p -> p.getPackageID().toString().contains(input)).collect(Collectors.toList());
                log.trace("filteredPackageMaintainers - result={}", result);
                return result;
            case "maintainerID":
                result =  StreamSupport.stream(packageMaintainers.spliterator(), false).filter(p -> p.getMaintainerID().toString().contains(input)).collect(Collectors.toList());
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