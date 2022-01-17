package ro.arc.packageManager.server.service;

import ro.arc.packageManager.common.domain.Maintainer;
import ro.arc.packageManager.common.domain.exceptions.AppException;
import ro.arc.packageManager.common.domain.exceptions.Contract;
import ro.arc.packageManager.common.domain.validators.ValidatorException;
import ro.arc.packageManager.common.service.IMaintainerService;
import ro.arc.packageManager.server.repository.Repository;

import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

public class MaintainerService implements IMaintainerService {

    private ExecutorService executorService;
    private Repository<Long, Maintainer> maintainerRepo;

    public MaintainerService(Repository<Long, Maintainer> maintainerRepo, ExecutorService executorService) {
        this.maintainerRepo = maintainerRepo;
        this.executorService = executorService;
    }

    @Override
    public Future<Void> addMaintainer(String userName, String fullName, String email) throws ValidatorException {
        Callable<Void> callable = () -> {
            Contract.ensure(
                    !existsMaintainerUserNameOrEmail(userName, email),
                    "A maintainer with that user name or email already exists");

            var maintainer = new Maintainer(0l, userName, fullName, email);

            this.maintainerRepo.save(maintainer);
            return null;
        };
        return this.executorService.submit(callable);
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
    public Future<Void> updateMaintainer(Long ID, String userName, String fullName, String email) throws ValidatorException {
        Callable<Void> callable = () -> {
            this.maintainerRepo.findOne(ID).orElseThrow(() -> new AppException("Non-existent maintainer with this ID !"));
            Contract.ensure(!existMaintainerUserNameDifferentID(userName, ID), "A maintainer with the new userName already exists.");
            Contract.ensure(!existMaintainerEmailDifferentID(email, ID), "A maintainer with the new email already exists.");

            var maintainer = new Maintainer(ID, userName, fullName, email);
            this.maintainerRepo.update(maintainer);
            return null;
        };
        return this.executorService.submit(callable);
    }

    @Override
    public Future<Void> deleteMaintainer(Long ID){
        Callable<Void> callable = () -> {
        this.maintainerRepo.findOne(ID).orElseThrow(() -> new AppException("Non-existent maintainer with this ID !"));
        this.maintainerRepo.delete(ID);
        return null;
        };
        return this.executorService.submit(callable);
    }

    @Override
    public Future<Stream<Maintainer>> getFilteredMaintainers(String type, String input) {
        var maintainers = this.maintainerRepo.findAll();
        switch (type) {
            case "userName":
                return this.executorService.submit(() -> StreamSupport.stream(maintainers.spliterator(), false).filter(m -> m.getUserName().contains(input)));
            case "fullName":
                return this.executorService.submit(() ->  StreamSupport.stream(maintainers.spliterator(), false).filter(m -> m.getFullName().contains(input)));
            case "email":
                return this.executorService.submit(() -> StreamSupport.stream(maintainers.spliterator(), false).filter(m -> m.getEmail().contains(input)));
        }
        throw new AppException("This type does not exist !");
    }

    @Override
    public Future<Stream<Maintainer>> getAllMaintainers() {
        var maintainers = this.maintainerRepo.findAll();
        return this.executorService.submit(() -> StreamSupport.stream(maintainers.spliterator(), false));
    }
}
