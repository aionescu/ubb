package packageManager.domain;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.assertEquals;

import ro.arc.packageManager.domain.PackageMaintainer;

public class PackageMaintainerTest {
  private static final Long id = 1l;
  private static final Long packageID = 3l;
  private static final Long newPackageID = 4l;
  private static final Long maintainerID = 5l;
  private static final Long newMaintainerID = 6l;

  private PackageMaintainer packageMaintainer;

  @Before
  public void setUp() {
    packageMaintainer = new PackageMaintainer(id, maintainerID, packageID);
  }

  @After
  public void tearDown() {
    packageMaintainer = null;
  }

  @Test
  public void testGetMaintainerID() {
    assertEquals("IDs should be equal", maintainerID, packageMaintainer.getMaintainerID());
  }

  @Test
  public void testSetMaintainerID() {
    packageMaintainer.setMaintainerID(newMaintainerID);
    assertEquals("IDs should be equal", newMaintainerID, packageMaintainer.getMaintainerID());
  }

  @Test
  public void testGetPackageID() {
    assertEquals("IDs should be equal", packageID, packageMaintainer.getPackageID());
  }

  @Test
  public void testSetPackageID() {
    packageMaintainer.setPackageID(newPackageID);
    assertEquals("IDs should be equal", newPackageID, packageMaintainer.getPackageID());
  }
}
