namespace WinFormApp {
  using System;
  using System.Collections.Generic;
  using System.ComponentModel;
  using System.Data;
  using System.Drawing;
  using System.Linq;
  using System.Text;
  using System.Threading.Tasks;
  using System.Windows.Forms;
  using System.Data.SqlClient;

  public partial class Form1: Form {
    SqlConnection _conn;
    DataSet _ds;
    SqlDataAdapter _daCustomers, _daOrders;
    SqlCommandBuilder _cb;
    BindingSource _bsCustomers, _bsOrders;

    public Form1() {
      InitializeComponent();
    }

    void Form1_Load(object sender, EventArgs e) {
      _conn = new SqlConnection(@"Data Source = localhost\SQLEXPRESS; Initial Catalog = Northwind; Integrated Security = SSPI;");
      _ds = new DataSet();

      _daCustomers = new SqlDataAdapter("select * from Customers2", _conn);
      _daOrders = new SqlDataAdapter("select * from Orders2", _conn);

      _cb = new SqlCommandBuilder(_daOrders);

      _daCustomers.Fill(_ds, "Customers");
      _daOrders.Fill(_ds, "Orders");

      var dr = new DataRelation(
        "fkOrdersCustomers",
        _ds.Tables["Customers"].Columns["cID"],
        _ds.Tables["Orders"].Columns["cID"]);

      Console.WriteLine(_ds.Tables["Customers"].Constraints.Count);
      Console.WriteLine(_ds.Tables["Orders"].Constraints.Count);

      _ds.Relations.Add(dr);
      // UniqueConstraint, ForeignKeyConstraint
      // GetChildRows, GetParentRow

      Console.WriteLine(_ds.Tables["Customers"].Constraints[0].GetType());
      Console.WriteLine(_ds.Tables["Orders"].Constraints[0].GetType());

      _bsCustomers = new BindingSource {
        DataSource = _ds,
        DataMember = "Customers"
      };

      _bsOrders = new BindingSource {
        DataSource = _bsCustomers,
        DataMember = "fkOrdersCustomers"
      };

      dgvCustomers.DataSource = _bsCustomers;
      dgvOrders.DataSource = _bsOrders;
    }

    void btnSaveData_Click(object sender, EventArgs e) {
      _daOrders.Update(_ds, "Orders");
    }
  }
}
