package snake.model;

import java.sql.*;
import java.util.Optional;

public class AuthManager extends DBManager {
  public AuthManager() {
    super();
  }

  public Optional<Integer> authenticate(String username, String password) {
    System.out.println(username + " " + password);

    var query = "select * from Users where username = ? and password = ?";

    try (var stmt = conn.prepareStatement(query)) {
      stmt.setString(1, username);
      stmt.setString(2, password);

      try (var result = stmt.executeQuery()) {
        if (result.next())
          return Optional.of(result.getInt("id"));
      }
    } catch (SQLException e) {
      e.printStackTrace();
    }

    return Optional.empty();
  }
}
