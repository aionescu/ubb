namespace Vacations.DataAbstraction {
  using System;
  using System.Collections.Generic;
  using System.Linq;
  using System.Web;
  using MySql.Data.MySqlClient;
  using Vacations.Models;

  public class DBDriver {
    private static MySqlConnection NewOpen() {
      MySqlConnection conn = new() {
        ConnectionString = "server=localhost;uid=root;pwd=;database=vacations;"
      };

      conn.Open();
      return conn;
    }

    public List<VacationDestination> GetAllDestinations() {
      using var conn = NewOpen();
      List<VacationDestination> dests = new();

      using MySqlCommand cmd = new() {
        Connection = conn,
        CommandText = $@"select * from VacationDestinations"
      };

      using var reader = cmd.ExecuteReader();

      while (reader.Read())
        dests.Add(new() {
          ID = reader.GetInt32("id"),
          Destination = reader.GetString("destination"),
          Country = reader.GetString("country"),
          Price = reader.GetInt32("price")
        });

      return dests;
    }

    public VacationDestination GetDestination(string destination) {
      using var conn = NewOpen();

      using MySqlCommand cmd = new() {
        Connection = conn,
        CommandText = $@"select * from VacationDestinations where destination = ""{destination}"""
      };

      using var reader = cmd.ExecuteReader();

      if (reader.Read())
        return new() {
          ID = reader.GetInt32("id"),
          Destination = reader.GetString("destination"),
          Country = reader.GetString("country"),
          Price = reader.GetInt32("price")
        };

      return null;
    }

    public void Ban(string user, string destination) {
      var dest = GetDestination(destination);
      using var conn = NewOpen();

      using MySqlCommand cmd = new() {
        Connection = conn,
        CommandText = $@"insert into BannedList(user, destinationID) values (""{user}"", ""{dest.ID}"")"
      };

      cmd.ExecuteNonQuery();
    }

    public bool IsBanned(string user, int dest) {
      using var conn = NewOpen();

      using MySqlCommand cmd = new() {
        Connection = conn,
        CommandText = $@"select * from BannedList where user = ""{user}"" and destinationID = {dest}"
      };

      using var reader = cmd.ExecuteReader();
      return reader.Read();
    }
  }
}
