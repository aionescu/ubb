package snake.model;

import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Timestamp;
import java.time.Instant;

public class GameManager extends DBManager {
  public GameManager() {
    super();
  }

  public int createGame(int userID) {
    var query = "insert into Games(player) values (?)";

    try (var stmt = conn.prepareStatement(query, Statement.RETURN_GENERATED_KEYS)) {
      stmt.setInt(1, userID);

      stmt.executeUpdate();

      try (var keys = stmt.getGeneratedKeys()) {
        if (keys.next())
            return keys.getInt(1);
      }
    } catch (SQLException e) {
      e.printStackTrace();
    }

    throw new RuntimeException("Couldn't add game.");
  }

  public void gameOver(int gameID, int score) {
    var query = "update Games set score = ?, endTime = ? where id = ?";

    try (var stmt = conn.prepareStatement(query)) {
      stmt.setInt(1, score);
      stmt.setTimestamp(2, Timestamp.from(Instant.now()));
      stmt.setInt(3, gameID);

      stmt.executeUpdate();
    } catch (SQLException e) {
      e.printStackTrace();
    }
  }

  public void makeMove(int gameID, int direction) {
    var query = "insert into Moves(game, direction) values (?, ?)";

    try (var stmt = conn.prepareStatement(query)) {
      stmt.setInt(1, gameID);
      stmt.setInt(2, direction);

      stmt.executeUpdate();
    } catch (SQLException e) {
      e.printStackTrace();
    }
  }
}
