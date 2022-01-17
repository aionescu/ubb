package ro.arc.packageManager.core.service;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ro.arc.packageManager.core.domain.Maintainer;
import ro.arc.packageManager.core.domain.exceptions.AppException;
import ro.arc.packageManager.core.domain.exceptions.Contract;
import ro.arc.packageManager.core.domain.validators.MaintainerValidator;
import ro.arc.packageManager.core.repository.MaintainerRepository;


import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

@Service
public class MaintainerServiceImpl implements MaintainerService{
    public static final Logger log = LoggerFactory.getLogger(MaintainerServiceImpl.class);

    @Autowired
    private MaintainerRepository maintainerRepo;

    @Autowired
    private MaintainerValidator validator;

    @Override
    @Transactional
    public Maintainer addMaintainer(Maintainer maintainer) {
        log.trace("addMaintainer - method entered: maintainer={}", maintainer);
        validator.validate(maintainer);
        var userNameOrEmail = maintainerRepo.existsAllByUserNameOrEmail(maintainer.getUserName(), maintainer.getEmail());
        Contract.ensure(!userNameOrEmail, "There is another maintainer with this userName or email");

        var result = this.maintainerRepo.save(maintainer);
        log.trace("addMaintainer - result={}", result);
        return result;
    }

    @Override
    @Transactional
    public Maintainer updateMaintainer(Maintainer maintainer)  {
        log.trace("updateMaintainer - method entered: maintainer={}", maintainer);
        var result = this.maintainerRepo.findById(maintainer.getId()).orElseThrow(() -> new AppException("Non-existent maintainer with this ID !"));
        var userNameDifferentId = maintainerRepo.existsAllByUserNameAndIdIsNot(maintainer.getUserName(), maintainer.getId());
        var emailDifferentId = maintainerRepo.existsAllByEmailAndIdIsNot(maintainer.getEmail(), maintainer.getId());
        Contract.ensure(!userNameDifferentId, "A maintainer with the new userName already exists.");
        Contract.ensure(!emailDifferentId, "A maintainer with the new email already exists.");


        result.setUserName(maintainer.getUserName());
        result.setFullName(maintainer.getFullName());
        result.setEmail(maintainer.getEmail());
        log.trace("updateMaintainer - result={}", result);
        return result;
    }

    @Override
    public void deleteMaintainer(Long ID){
        log.trace("deleteMaintainer - method entered : id={}", ID);
        this.maintainerRepo.findById(ID).orElseThrow(() -> new AppException("Non-existent maintainer with this ID !"));
        this.maintainerRepo.deleteById(ID);
        log.trace("deleteMaintainer - method finished");
    }

    @Override
    public List<Maintainer> getFilteredMaintainers(String type, String input) {
        log.trace("filteredMaintainers - method entered : type={}, input={}",type, input);
        List<Maintainer> result;
        var maintainers = this.maintainerRepo.findAll();
        switch (type) {
            case "userName":
                result = maintainerRepo.findAllByUserNameContains(input);
                log.trace("filteredMaintainers - result={}", result);
                return result;
            case "fullName":
                result = maintainerRepo.findAllByFullNameContains(input);
                log.trace("filteredMaintainers - result={}", result);
                return result;
            case "email":
                result = maintainerRepo.findAllByEmailContains(input);
                log.trace("filteredMaintainers - result={}", result);
                return result;
        }
        throw new AppException("This type does not exist !");
    }

    @Override
    public List<Maintainer> getAllMaintainers() {
        log.trace("allMaintainers - method entered");
        var maintainers = this.maintainerRepo.findAll();
        log.trace("allMaintainers - result={}", maintainers);
        return new ArrayList<>(maintainers);
    }
}
