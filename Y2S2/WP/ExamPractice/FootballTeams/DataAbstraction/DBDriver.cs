namespace FootballTeams.DataAbstraction {
  using System;
  using System.Collections.Generic;
  using System.Linq;
  using System.Web;
  using FootballTeams.Models;
  using MySql.Data.MySqlClient;

  public class DBDriver {
    private static MySqlConnection NewOpen() {
      MySqlConnection conn = new() {
        ConnectionString = "server=localhost;uid=root;pwd=;database=football_teams;"
      };

      conn.Open();
      return conn;
    }

    public List<Team> GetAllTeams() {
      using var conn = NewOpen();

      List<Team> teams = new();

      using MySqlCommand cmd = new() {
        Connection = conn,
        CommandText = "select * from Teams"
      };

      using var reader = cmd.ExecuteReader();

      while (reader.Read())
        teams.Add(new() {
          ID = reader.GetInt32("id"),
          CaptainID = reader.GetInt32("captainID"),
          Name = reader.GetString("name"),
          Description = reader.GetString("description"),
          MembersString = reader.GetString("members")
        });

      return teams;
    }

    public Team GetTeam(string name) {
      using var conn = NewOpen();

      using MySqlCommand cmd = new() {
        Connection = conn,
        CommandText = $@"select * from Teams where name = ""{name}"""
      };

      using var reader = cmd.ExecuteReader();

      if (reader.Read())
        return new() {
          ID = reader.GetInt32("id"),
          CaptainID = reader.GetInt32("captainID"),
          Name = reader.GetString("name"),
          Description = reader.GetString("description"),
          MembersString = reader.GetString("members")
        };

      return null;
    }

    public Player GetPlayer(string name) {
      using var conn = NewOpen();

      using MySqlCommand cmd = new() {
        Connection = conn,
        CommandText = $@"select * from Players where name = ""{name}"""
      };

      using var reader = cmd.ExecuteReader();

      if (reader.Read())
        return new() {
          ID = reader.GetInt32("id"),
          Name = reader.GetString("name"),
          Age = reader.GetInt32("age")
        };

      return null;
    }

    public void AddTeam(Team team) {
      using var conn = NewOpen();

      using MySqlCommand cmd = new() {
        Connection = conn,
        CommandText = $@"insert into Teams(captainID, name, description, members) values ({team.CaptainID}, ""{team.Name}"", ""{team.Description}"", ""{team.MembersString}"")"
      };

      cmd.ExecuteNonQuery();
      team.ID = (int)cmd.LastInsertedId;
    }

    public void UpdateTeam(Team team) {
      using var conn = NewOpen();

      using MySqlCommand cmd = new() {
        Connection = conn,
        CommandText = $@"update Teams set captainID = {team.CaptainID}, name = ""{team.Name}"", description = ""{team.Description}"", members = ""{team.MembersString}"" where id = {team.ID}"
      };

      cmd.ExecuteNonQuery();
    }
  }
}
