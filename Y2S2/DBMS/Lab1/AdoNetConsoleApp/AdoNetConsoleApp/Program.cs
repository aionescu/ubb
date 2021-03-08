namespace AdoNetConsoleApp {
  using System;
  using System.Data;
  using System.Data.SqlClient;

  // SqlConnection, SqlCommand, SqlDataReader, Sqladapter
  // DataSet -> DataTable
  // DataTable -> DataRow, DataColumn
  // SqlCommandBuilder

  // AdventureWorks, Northwind, pubs - Sample SQL Server DBs
  // .mdf, .ldf - DB w/ Log file
  // .sql - DB creation & population script

  // sabina@cs.ubbcluj.ro -> Attendance files
  // sabinacsen@gmail.com -> Lab HW

  // SqlCommand - ExecuteReader, ExecuteNonQuery
  // Sqladapter,DataSet - add row, update row , eliminate row(delete/remove)
  // SqlCommandBuilder

  static class Program {
    static void Main(string[] args) {
      var conn = new SqlConnection {
        ConnectionString = @"Data Source = localhost\SQLEXPRESS; Initial Catalog = Northwind; Integrated Security = SSPI;"
      };

      var dataSet = new DataSet();

      var adapter = new SqlDataAdapter("select * from Region", conn);
      adapter.Fill(dataSet, "Region");

      var builder = new SqlCommandBuilder(adapter);
      builder.GetInsertCommand();

      //adapter.InsertCommand = new SqlCommand(
      //  "insert into Region(RegionID, RegionDescription) values (@RegionID, @RegionDescription)",
      //  conn);

      //adapter.InsertCommand.Parameters.Add("@RegionID", SqlDbType.Int, 10, "RegionID");
      //adapter.InsertCommand.Parameters.Add("@RegionDescription", SqlDbType.NVarChar, 50, "RegionDescription");

      var tbl = dataSet.Tables["Region"];

      foreach (DataRow row in tbl.Rows)
        Console.WriteLine($"{row["RegionID"]}: {row["RegionDescription"]}");

      var newRow = tbl.NewRow();

      newRow["RegionID"] = 7;
      newRow["RegionDescription"] = "North-East";

      tbl.Rows.Add(newRow);
      adapter.Update(tbl);

      Console.WriteLine();
      foreach (DataRow row in tbl.Rows)
        Console.WriteLine($"{row["RegionID"]}: {row["RegionDescription"]}");

      //conn.Open();
      //var cmd = conn.CreateCommand();
      //cmd.CommandText = "insert into Region values (6, 'North-West')";
      //cmd.ExecuteNonQuery();

      Console.ReadKey();
    }
  }
}
