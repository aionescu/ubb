package ro.arc.packageManager.service;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import ro.arc.packageManager.domain.Maintainer;
import ro.arc.packageManager.domain.exceptions.AppException;
import ro.arc.packageManager.domain.exceptions.Contract;
import ro.arc.packageManager.domain.validators.MaintainerValidator;
import ro.arc.packageManager.repository.MaintainerRepository;

import javax.transaction.Transactional;
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
    public void addMaintainer(Maintainer maintainer) {
        log.trace("addMaintainer - method entered: maintainer={}", maintainer);
        validator.validate(maintainer);
        Contract.ensure(
                !existsMaintainerUserNameOrEmail(maintainer.getUserName(), maintainer.getEmail()),
                "A maintainer with that user name or email already exists");

        this.maintainerRepo.save(maintainer);
        log.trace("addMaintainer - method finished");
    }
    private boolean existsMaintainerUserNameOrEmail(String userName, String email) {
        Contract.notNull(userName, "userName");
        Contract.notNull(email, "email");

        var all = this.maintainerRepo.findAll();

        return
                StreamSupport.stream(all.spliterator(), false)
                        .anyMatch(m -> m.getUserName().equals(userName) || m.getEmail().equals(email));
    }

    private boolean existMaintainerUserNameDifferentID(String userName, Long ID)
    {
        Contract.notNull(userName, "name");

        var all = maintainerRepo.findAll();

        return StreamSupport.stream(all.spliterator(), false).anyMatch(m -> m.getUserName().equals(userName) && m.getID() != ID);
    }

    private boolean existMaintainerEmailDifferentID(String email, Long ID)
    {
        Contract.notNull(email, "name");

        var all = maintainerRepo.findAll();

        return StreamSupport.stream(all.spliterator(), false).anyMatch(m -> m.getUserName().equals(email) && m.getID() != ID);
    }

    @Override
    @Transactional
    public void updateMaintainer(Maintainer maintainer)  {
        log.trace("updateMaintainer - method entered: maintainer={}", maintainer);
        this.maintainerRepo.findById(maintainer.getID()).orElseThrow(() -> new AppException("Non-existent maintainer with this ID !"));
        Contract.ensure(!existMaintainerUserNameDifferentID(maintainer.getUserName(), maintainer.getID()), "A maintainer with the new userName already exists.");
        Contract.ensure(!existMaintainerEmailDifferentID(maintainer.getEmail(), maintainer.getID()), "A maintainer with the new email already exists.");



        this.maintainerRepo.findById(maintainer.getID()).
                ifPresent(assig -> {
                    assig.setUserName(maintainer.getUserName());
                    assig.setFullName(maintainer.getFullName());
                    assig.setEmail(maintainer.getEmail());
                        }
                );
        log.trace("updateMaintainer - method finished");
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
                result =  StreamSupport.stream(maintainers.spliterator(), false).filter(m -> m.getUserName().contains(input)).collect(Collectors.toList());
                log.trace("filteredMaintainers - result={}", result);
                return result;
            case "fullName":
                result =  StreamSupport.stream(maintainers.spliterator(), false).filter(m -> m.getFullName().contains(input)).collect(Collectors.toList());
                log.trace("filteredMaintainers - result={}", result);
                return result;
            case "email":
                result =  StreamSupport.stream(maintainers.spliterator(), false).filter(m -> m.getEmail().contains(input)).collect(Collectors.toList());
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
