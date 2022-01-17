package packageManager.domain;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.assertEquals;

import ro.arc.packageManager.domain.Maintainer;

public class MaintainerTest {
  private static final Long id = 1l;
  private static final Long newID = 2l;
  private static final String userName = "m01";
  private static final String newUserName = "m02";
  private static final String fullName = "f01";
  private static final String newFullName = "f02";
  private static final String email = "m01@g.gg";
  private static final String newEmail = "m02@g.gg";

  private Maintainer maintainer;

  @Before
  public void setUp() {
    maintainer = new Maintainer(id, userName, fullName, email);
  }

  @After
  public void tearDown() {
    maintainer = null;
  }

  @Test
  public void testGetID() {
    assertEquals("IDs should be equal", id, maintainer.getID());
  }

  @Test
  public void testSetID() {
    maintainer.setID(newID);
    assertEquals("IDs should be equal", newID, maintainer.getID());
  }

  @Test
  public void testGetUserName() {
    assertEquals("UserNames should be equal", userName, maintainer.getUserName());
  }

  @Test
  public void testSetUserName() {
    maintainer.setUserName(newUserName);
    assertEquals("UserNames should be equal", newUserName, maintainer.getUserName());
  }

  @Test
  public void testGetFullName() {
    assertEquals("FullNames should be equal", fullName, maintainer.getFullName());
  }

  @Test
  public void testSetFullName() {
    maintainer.setFullName(newFullName);
    assertEquals("FullNames should be equal", newFullName, maintainer.getFullName());
  }

  @Test
  public void testGetEmail() {
    assertEquals("Emails should be equal", email, maintainer.getEmail());
  }

  @Test
  public void testSetEmail() {
    maintainer.setEmail(newEmail);
    assertEquals("Emails should be equal", newEmail, maintainer.getEmail());
  }
}
