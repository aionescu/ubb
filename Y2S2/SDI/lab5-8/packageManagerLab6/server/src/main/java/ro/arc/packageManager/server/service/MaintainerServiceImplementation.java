package ro.arc.packageManager.server.service;

import ro.arc.packageManager.common.domain.Maintainer;
import ro.arc.packageManager.common.domain.exceptions.AppException;
import ro.arc.packageManager.common.domain.exceptions.Contract;
import ro.arc.packageManager.common.service.MaintainerService;
import ro.arc.packageManager.server.repository.Repository;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

public class MaintainerServiceImplementation implements MaintainerService {

    private Repository<Long, Maintainer> maintainerRepo;

    public MaintainerServiceImplementation(Repository<Long, Maintainer> maintainerRepo) {
        this.maintainerRepo = maintainerRepo;
    }

    @Override
    public void addMaintainer(Maintainer maintainer) {
        Contract.ensure(
                !existsMaintainerUserNameOrEmail(maintainer.getUserName(), maintainer.getEmail()),
                "A maintainer with that user name or email already exists");

        this.maintainerRepo.save(maintainer);
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
    public void updateMaintainer(Maintainer maintainer)  {
        this.maintainerRepo.findOne(maintainer.getID()).orElseThrow(() -> new AppException("Non-existent maintainer with this ID !"));
        Contract.ensure(!existMaintainerUserNameDifferentID(maintainer.getUserName(), maintainer.getID()), "A maintainer with the new userName already exists.");
        Contract.ensure(!existMaintainerEmailDifferentID(maintainer.getEmail(), maintainer.getID()), "A maintainer with the new email already exists.");

        this.maintainerRepo.update(maintainer);
    }

    @Override
    public void deleteMaintainer(Long ID){
        this.maintainerRepo.findOne(ID).orElseThrow(() -> new AppException("Non-existent maintainer with this ID !"));
        this.maintainerRepo.delete(ID);
    }

    @Override
    public List<Maintainer> getFilteredMaintainers(String type, String input) {
        var maintainers = this.maintainerRepo.findAll();
        switch (type) {
            case "userName":
                return StreamSupport.stream(maintainers.spliterator(), false).filter(m -> m.getUserName().contains(input)).collect(Collectors.toList());
            case "fullName":
                return StreamSupport.stream(maintainers.spliterator(), false).filter(m -> m.getFullName().contains(input)).collect(Collectors.toList());
            case "email":
                return StreamSupport.stream(maintainers.spliterator(), false).filter(m -> m.getEmail().contains(input)).collect(Collectors.toList());
        }
        throw new AppException("This type does not exist !");
    }

    @Override
    public List<Maintainer> getAllMaintainers() {
        var maintainers = this.maintainerRepo.findAll();
        List<Maintainer> toReturn = new ArrayList<>();
        maintainers.forEach(toReturn::add);
        return toReturn;
    }
}
