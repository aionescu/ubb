package ro.arc.packageManager.service;

import ro.arc.packageManager.domain.Maintainer;
import ro.arc.packageManager.domain.exceptions.AppException;
import ro.arc.packageManager.domain.exceptions.Contract;
import ro.arc.packageManager.domain.validators.ValidatorException;
import ro.arc.packageManager.repository.Repository;

import java.util.stream.Stream;
import java.util.stream.StreamSupport;

public class MaintainerService {
  private Repository<Long, Maintainer> maintainerRepo;

  public MaintainerService(Repository<Long, Maintainer> maintainerRepo) {
    this.maintainerRepo = maintainerRepo;
  }

  public void addMaintainer(String userName, String fullName, String email) throws ValidatorException {
    Contract.ensure(
      !existsMaintainerUserNameOrEmail(userName, email),
      "A maintainer with that user name or email already exists");

    var maintainer = new Maintainer(0l, userName, fullName, email);

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


  public void updateMaintainer(Long ID, String userName, String fullName, String email) throws ValidatorException {
    this.maintainerRepo.findOne(ID).orElseThrow(() -> new AppException("Non-existent maintainer with this ID !"));
    Contract.ensure(!existMaintainerUserNameDifferentID(userName, ID), "A maintainer with the new userName already exists.");
    Contract.ensure(!existMaintainerEmailDifferentID(email, ID), "A maintainer with the new email already exists.");

    var maintainer = new Maintainer(ID,userName,fullName,email);
    this.maintainerRepo.update(maintainer);
  }

  public void deleteMaintainer(Long ID){
    this.maintainerRepo.findOne(ID).orElseThrow(() -> new AppException("Non-existent maintainer with this ID !"));
    this.maintainerRepo.delete(ID);
  }

  public Stream<Maintainer> getFilteredMaintainers(String type, String input) {
    var maintainers = this.maintainerRepo.findAll();
    switch (type) {
      case "userName":
        return StreamSupport.stream(maintainers.spliterator(), false).filter(m -> m.getUserName().contains(input));
      case "fullName":
        return StreamSupport.stream(maintainers.spliterator(), false).filter(m -> m.getFullName().contains(input));
      case "email":
        return StreamSupport.stream(maintainers.spliterator(), false).filter(m -> m.getEmail().contains(input));
    }
    throw new AppException("This type does not exist !");
  }


  public Stream<Maintainer> getAllMaintainers() {
    var maintainers = this.maintainerRepo.findAll();
    return StreamSupport.stream(maintainers.spliterator(), false);
  }


}
