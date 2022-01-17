namespace Exam.DataAbstraction {
  using System;
  using System.Collections.Generic;
  using System.Linq;
  using System.Web;
  using Exam.Models;
  using MySql.Data.MySqlClient;

  public enum DeletionResult {
    Successful,
    HasBooks,
    Inexistent
  }

  public class DBDriver {
    private static MySqlConnection NewOpen() {
      MySqlConnection conn = new() {
        ConnectionString = "server=localhost;uid=root;pwd=;database=exam;"
      };

      conn.Open();
      return conn;
    }

    public List<Book> GetAllBooks() {
      using var conn = NewOpen();

      List<Book> books = new();

      using MySqlCommand cmd = new() {
        Connection = conn,
        CommandText = "select * from Books"
      };

      using var reader = cmd.ExecuteReader();

      while (reader.Read())
        books.Add(new() {
          ID = reader.GetInt32("id"),
          IDPublishingHouse = reader.GetInt32("idPublishingHouse"),
          Name = reader.GetString("name"),
          Topic1 = reader.GetString("topic1"),
          Topic2 = reader.GetString("topic2"),
          Topic3 = reader.GetString("topic3"),
          Topic4 = reader.GetString("topic4"),
          Topic5 = reader.GetString("topic5")
        });

      return books;
    }

    public List<PublishingHouse> GetAllPublishingHouses() {
      using var conn = NewOpen();

      List<PublishingHouse> houses = new();

      using MySqlCommand cmd = new() {
        Connection = conn,
        CommandText = "select * from PublishingHouses"
      };

      using var reader = cmd.ExecuteReader();

      while (reader.Read())
        houses.Add(new() {
          ID = reader.GetInt32("id"),
          Name = reader.GetString("name"),
          URL = reader.GetString("url")
        });

      return houses;
    }

    public PublishingHouse GetPublishingHouse(string name) {
      using var conn = NewOpen();

      using MySqlCommand cmd = new() {
        Connection = conn,
        CommandText = $@"select * from PublishingHouses where name = ""{name}"""
      };

      using var reader = cmd.ExecuteReader();

      if (reader.Read())
        return new() {
          ID = reader.GetInt32("id"),
          Name = reader.GetString("name"),
          URL = reader.GetString("url")
        };

      return null;
    }

    public DeletionResult DeletePublishingHouse(string name) {
      var house = GetPublishingHouse(name);

      if (house is null)
        return DeletionResult.Inexistent;

      if (GetBookCount(house.ID) > 0)
        return DeletionResult.HasBooks;

      using var conn = NewOpen();

      using MySqlCommand cmd = new() {
        Connection = conn,
        CommandText = $"delete from PublishingHouses where id = {house.ID}"
      };

      cmd.ExecuteNonQuery();
      return DeletionResult.Successful;
    }

    public int GetBookCount(int idPublishingHouse) {
      using var conn = NewOpen();

      using MySqlCommand cmd = new() {
        Connection = conn,
        CommandText = $"select COUNT(*) from Books where idPublishingHouse = {idPublishingHouse}"
      };

      return (int)(long)cmd.ExecuteScalar();
    }
  }
}
