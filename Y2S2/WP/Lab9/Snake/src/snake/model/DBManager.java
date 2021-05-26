package snake.model;

import java.sql.*;

public abstract class DBManager {
  protected Connection conn;

  public DBManager() {
    connect();
  }

  private void connect() {
    try {
      Class.forName("org.gjt.mm.mysql.Driver");
      conn = DriverManager.getConnection("jdbc:mysql://127.0.0.1/snake", "root", "");
    } catch (Exception e) {
      System.out.println("Error while connecting: " + e.getMessage());
      e.printStackTrace();
    }
  }
}
