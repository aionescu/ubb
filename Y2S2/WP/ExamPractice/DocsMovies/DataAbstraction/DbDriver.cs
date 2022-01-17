namespace DocsMovies.DataAbstraction {
  using System;
  using System.Collections.Generic;
  using System.Linq;
  using System.Web;
  using DocsMovies.Models;
  using MySql.Data.MySqlClient;

  public class DbDriver
  {
    private static MySqlConnection NewOpen() {
      MySqlConnection conn = new() {
        ConnectionString = "server=localhost;uid=root;pwd=;database=docs_movies;"
      };

      conn.Open();
      return conn;
    }

    public List<Author> GetAllAuthors() {
      using var conn = NewOpen();

      List<Author> authors = new();

      using MySqlCommand cmd = new() {
        Connection = conn,
        CommandText = "select * from Authors"
      };

      using var reader = cmd.ExecuteReader();

      while (reader.Read())
        authors.Add(new() {
          Id = reader.GetInt32("id"),
          Name = reader.GetString("name"),
          DocumentList = reader.GetString("documentList"),
          MovieList = reader.GetString("movieList")
        });

      return authors;
    }

    public Author GetAuthor(string name) {
      using var conn = NewOpen();

      using MySqlCommand cmd = new() {
        Connection = conn,
        CommandText = $@"select * from Authors where name = ""{name}"""
      };

      using var reader = cmd.ExecuteReader();

      if (reader.Read())
        return new() {
          Id = reader.GetInt32("id"),
          Name = reader.GetString("name"),
          DocumentList = reader.GetString("documentList"),
          MovieList = reader.GetString("movieList")
        };

      throw new Exception("Author not found.");
    }

    public Document GetDocument(int id) {
      using var conn = NewOpen();

      using MySqlCommand cmd = new() {
        Connection = conn,
        CommandText = $@"select * from Documents where id = {id}"
      };

      using var reader = cmd.ExecuteReader();

      if (reader.Read())
        return new() {
          Id = reader.GetInt32("id"),
          Name = reader.GetString("name"),
          Contents = reader.GetString("contents")
        };

      throw new Exception("Document not found.");
    }

    public Movie GetMovie(int id) {
      using var conn = NewOpen();

      using MySqlCommand cmd = new() {
        Connection = conn,
        CommandText = $@"select * from Movies where id = {id}"
      };

      using var reader = cmd.ExecuteReader();

      if (reader.Read())
        return new() {
          Id = reader.GetInt32("id"),
          Title = reader.GetString("title"),
          Duration = reader.GetInt32("duration")
        };

      throw new Exception("Movie not found.");
    }

    public void AddDocument(Document doc) {
      using var conn = NewOpen();

      using MySqlCommand cmd = new() {
        Connection = conn,
        CommandText = $@"insert into Documents(name, contents) values (""{doc.Name}"", ""{doc.Contents}"")"
      };

      Console.WriteLine(cmd.ExecuteNonQuery());

      doc.Id = (int)cmd.LastInsertedId;
    }
  }
}
