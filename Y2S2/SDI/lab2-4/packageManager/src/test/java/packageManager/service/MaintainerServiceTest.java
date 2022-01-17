package packageManager.service;

import java.util.*;
import java.util.stream.Collectors;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.*;

import ro.arc.packageManager.domain.Maintainer;
import ro.arc.packageManager.domain.validators.MaintainerValidator;
import ro.arc.packageManager.repository.InMemoryRepository;
import ro.arc.packageManager.repository.Repository;
import ro.arc.packageManager.service.MaintainerService;

public class MaintainerServiceTest {
  private Repository<Long, Maintainer> maintainerRepo;
  private MaintainerValidator maintainerValidator = new MaintainerValidator();
  private MaintainerService maintainerService;

  @Before
  public void setUp() throws Exception {
    maintainerRepo = new InMemoryRepository<>(maintainerValidator);
    maintainerService = new MaintainerService(maintainerRepo);
  }

  @After
  public void tearDown() throws Exception {
    maintainerRepo = null;
    maintainerService = null;
  }

  @Test
  public void addMaintainer() {
    @SuppressWarnings("unused")
    Maintainer maintainer = new Maintainer(1l, "userName1", "fullName1", "e@mail.com");
    maintainerService.addMaintainer("userName1", "fullName1", "e@mail.com");
    assertEquals("Maintainer should be in repo", maintainerService.getAllMaintainers().count(), 1);
  }

  @Test
  public void getAllMaintainers() {
    Maintainer maintainer1 = new Maintainer(0l, "userName1", "fullName1", "e1@mail.com");
    Maintainer maintainer2 = new Maintainer(1l, "userName2", "fullName2", "e2@mail.com");
    maintainerService.addMaintainer("userName1", "fullName1", "e1@mail.com");
    maintainerService.addMaintainer("userName2", "fullName2", "e2@mail.com");
    HashSet<Maintainer> hashSet = new HashSet<Maintainer>();
    hashSet.add(maintainer1);
    hashSet.add(maintainer2);
    assertEquals("Maintainers should be equal",
      maintainerService.getAllMaintainers().collect(Collectors.toCollection(HashSet::new)), hashSet);
  }

  @Test
  public void deleteMaintainer() {
    Maintainer maintainer1 = new Maintainer(0l, "userName1", "fullName1", "e1@mail.com");
    maintainerService.addMaintainer("userName1", "fullName1", "e1@mail.com");
    maintainerService.addMaintainer("userName2", "fullName2", "e2@mail.com");
    HashSet<Maintainer> hashSet = new HashSet<Maintainer>();
    hashSet.add(maintainer1);
    maintainerService.deleteMaintainer(1l);
    assertEquals("Maintainers should be equal",
      maintainerService.getAllMaintainers().collect(Collectors.toCollection(HashSet::new)), hashSet);
  }

  @Test
  public void updateMaintainer() {
    Maintainer maintainer1 = new Maintainer(0l, "newUserName", "newFullName", "new@mail.com");
    maintainerService.addMaintainer("userName1", "fullName1", "e1@mail.com");
    HashSet<Maintainer> hashSet = new HashSet<Maintainer>();
    hashSet.add(maintainer1);
    maintainerService.updateMaintainer(0l, "newUserName", "newFullName", "new@mail.com");
    assertEquals("Maintainers should be equal",
      maintainerService.getAllMaintainers().collect(Collectors.toCollection(HashSet::new)), hashSet);
  }

  @Test
  public void getFilteredMaintainers() {
    maintainerService.addMaintainer("userName1", "fullName1", "e1@mail.com");
    maintainerService.addMaintainer("userName12", "fullName12", "e12@mail.com");
    maintainerService.addMaintainer("userName23", "fullName23", "e23@mail.com");
    maintainerService.addMaintainer("userName2", "fullName2", "e2@mail.com");
    maintainerService.addMaintainer("userName5", "fullName5", "e5@mail.com");

    Maintainer maintainer1 = new Maintainer(0l, "userName1", "fullName1", "e1@mail.com");
    Maintainer maintainer2 = new Maintainer(1l, "userName12", "fullName12", "e12@mail.com");

    HashSet<Maintainer> hashSet = new HashSet<Maintainer>();
    hashSet.add(maintainer1);
    hashSet.add(maintainer2);

    assertEquals("Maintainers should be equal",
      maintainerService.getFilteredMaintainers("userName", "1").collect(Collectors.toCollection(HashSet::new)),
      hashSet);
  }
}
