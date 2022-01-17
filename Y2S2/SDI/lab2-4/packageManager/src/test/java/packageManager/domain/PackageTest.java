package packageManager.domain;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.assertEquals;

import ro.arc.packageManager.domain.Package;

public class PackageTest {
  private static final Long id = 1l;
  private static final String name = "p01";
  private static final String newName = "p02";
  private static final String description = "d01";
  private static final String newDescription = "d02";
  private static final String sourceRepo = "https://s01.com";
  private static final String newSourceRepo = "https://s02.com";
  private static final String license = "l01";
  private static final String newLicense = "l02";

  private Package pack;

  @Before
  public void setUp() {
    pack = new Package(id, name, description, sourceRepo, license);
  }

  @After
  public void tearDown() {
    pack = null;
  }

  @Test
  public void testGetName() {
    assertEquals("Names should be equal", name, pack.getName());
  }

  @Test
  public void testSetName() {
    pack.setName(newName);
    assertEquals("Names should be equal", newName, pack.getName());
  }

  @Test
  public void testGetDescription() {
    assertEquals("Descriptions should be equal", description, pack.getDescription());
  }

  @Test
  public void testSetDescription() {
    pack.setDescription(newDescription);
    assertEquals("Description should be equal", newDescription, pack.getDescription());
  }

  @Test
  public void testGetSourceRepo() {
    assertEquals("SourceRepos should be equal", sourceRepo, pack.getSourceRepo());
  }

  @Test
  public void testSetSourceRepo() {
    pack.setSourceRepo(newSourceRepo);
    assertEquals("SourceRepos should be equal", newSourceRepo, pack.getSourceRepo());
  }

  @Test
  public void testGetLicense() {
    assertEquals("Licenses should be equal", license, pack.getLicense());
  }

  @Test
  public void testSetLicense() {
    pack.setLicense(newLicense);
    assertEquals("Licenses should be equal", newLicense, pack.getLicense());
  }
}
