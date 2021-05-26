package snake.controller;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import snake.model.AuthManager;
import snake.model.GameManager;
import snake.model.User;

public class Login extends HttpServlet {
  public Login() {
    super();
  }

  protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
    var username = request.getParameter("username");
    var password = request.getParameter("password");

    var auth = new AuthManager();
    var result = auth.authenticate(username, password);

    if (result.isEmpty()) {
      request.getRequestDispatcher("/loginFailed.jsp").forward(request, response);
      return;
    }

    var user = new User(result.get(), username, password);

    var session = request.getSession();
    session.setAttribute("user", user);

    var game = new GameManager();
    var gameID = game.createGame(result.get());
    session.setAttribute("gameID", gameID);

    request.getRequestDispatcher("/game.jsp").forward(request, response);
  }
}
