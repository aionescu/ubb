package snake.controller;

import snake.model.GameManager;
import snake.model.User;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

public class GameOver extends HttpServlet {
  public GameOver() {
    super();
  }

  @Override
  protected void doPost(HttpServletRequest request, HttpServletResponse response) throws IOException {
    var score = Integer.parseInt(request.getParameter("score"));

    var session = request.getSession();
    var user = (User)session.getAttribute("user");
    int gameID = (Integer)session.getAttribute("gameID");

    if (user == null) {
      response.setStatus(401);
      return;
    }

    var game = new GameManager();
    game.gameOver(gameID, score);

    response.setStatus(200);
  }
}
