package packageManager.repository;

import java.util.HashSet;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.*;

import ro.arc.packageManager.domain.Maintainer;
import ro.arc.packageManager.domain.exceptions.AppException;
import ro.arc.packageManager.domain.validators.MaintainerValidator;
import ro.arc.packageManager.repository.InMemoryRepository;
import ro.arc.packageManager.repository.Repository;

public class InMemoryRepositoryTest {
  private Repository<Long, Maintainer> maintainerRepo;
  private MaintainerValidator maintainerValidator = new MaintainerValidator();

  @Before
  public void setUp() {
    maintainerRepo = new InMemoryRepository<>(maintainerValidator);
  }

  @After
  public void tearDown() {
    maintainerRepo = null;
  }

  @Test
  public void testFindOne() {
    Maintainer maintainer1 = new Maintainer(1l, "userName1", "fullName1", "e@mail.com");
    maintainerRepo.save(maintainer1);

    assertEquals("Maintainer ID's should be equal", maintainerRepo.findOne(1l).get(), maintainer1);
  }

  @Test
  public void testFindAll() throws Exception {
    Maintainer maintainer1 = new Maintainer(1l, "userName1", "fullName1", "e@mail.com");
    Maintainer maintainer2 = new Maintainer(2l, "userName2", "fullName2", "e@mail.com");
    maintainerRepo.save(maintainer1);
    maintainerRepo.save(maintainer2);

    HashSet<Maintainer> hashSet = new HashSet<Maintainer>();
    hashSet.add(maintainer1);
    hashSet.add(maintainer2);

    assertEquals("Maintainer ID's should be equal", maintainerRepo.findAll(), hashSet);
  }

  @Test
  public void testSave() throws Exception {
    Maintainer maintainer1 = new Maintainer(1l, "userName1", "fullName1", "e@mail.com");
    assertEquals("Repo size should be 0", 0, maintainerRepo.findAll().spliterator().getExactSizeIfKnown());
    maintainerRepo.save(maintainer1);
    assertEquals("Repo size should be 1", 1, maintainerRepo.findAll().spliterator().getExactSizeIfKnown());
  }

  @Test(expected = AppException.class)
  public void testSaveException() throws AppException {
    Maintainer maintainer1 = null;
    maintainerRepo.save(maintainer1);
  }
}
