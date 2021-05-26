<%@ page import="java.util.List" %>
<%@ page import="snake.model.Table" %>
<%@ page import="snake.model.Snake" %>
<%@ page import="snake.model.User" %>
<%@ page import="javax.swing.*" %>
<%@ page contentType="text/html;charset=UTF-8" language="java" %>

<html>
  <head>
    <link href="gameOld.css" rel="stylesheet" type="text/css">
    <title>SNAKE</title>
  </head>
  <body>
    SNAKE
    <%  int state = (int)session.getAttribute("state");
        if(state == -1){
    %>
    <div class="game-over">GAME OVER!!!</div>

    <%}
        else{%>
    <%!  Table table; %>
    <%  table = (Table) session.getAttribute("table");
        User user = (User) session.getAttribute("user");
        List<Integer> positions = user.getSnake().positions();
        for (int p = 0; p < positions.size()-1; p+= 2){
            int  i = positions.get(p);
            int j = positions.get(p+1);
            table.getTable()[i][j] = 3;
        };
    %>

    <div class="table">
        <%for(int i = 0 ; i < table.getRows(); i++){
              for(int j = 0; j < table.getColumns(); j++) { %>
            <%  int cell = table.get(i, j);
                if (cell == 0){
            %>
                <div class="empty"></div>
            <%}
                if (cell == 1){
            %>
                <div class="obstacle"></div>
            <%}
                if (cell == 3){
            %>
                <div class="snake"></div>
        <%}}%>
        <%}%>
    </div>

    <%
        for (int p = 0; p < positions.size()-1; p+= 2){
            int i = positions.get(p);
            int j = positions.get(p+1);
            table.getTable()[i][j] = 0;
        };
    %>

    <form class="keys" action="Game" method="post">
        <input type="submit" class="up arr" type="submit" name="up" value="^">
        <br/>
        <input type="submit" class="left arr" name="left" value="<">
        <input type="submit" class="down arr" name="down" value="v">
        <input type="submit" class="right arr" name="right" value=">">
        <br/>
        <input type="submit" name="close" value="Close">
    </form>
    <%}%>
    <br>

    <div class="scoreboard">
        SCORE: <%out.println(session.getAttribute("score"));%>
        TIMING: <%out.println((System.currentTimeMillis() - (long)session.getAttribute("start-time"))/1000);%>
    </div>
  </body>
</html>
