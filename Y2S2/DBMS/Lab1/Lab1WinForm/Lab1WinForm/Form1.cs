namespace Lab1WinForm {
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
    DataSet _dataSet;
    SqlDataAdapter _pkgAdapter, _pkgVerAdapter;
    SqlCommandBuilder _cmdBuilder;
    BindingSource _pkgBindingSource, _pkgVerBindingSourcew;

    public Form1() {
      InitializeComponent();

      _conn = new SqlConnection(@"Data Source = localhost\SQLEXPRESS; Initial Catalog = PackageManager; Integrated Security = SSPI;");

      _initialize();
      _conn.Open();
    }

    void _initialize() {
      _dataSet = new DataSet();

      _pkgAdapter = new SqlDataAdapter("select * from Packages", _conn);
      _pkgVerAdapter = new SqlDataAdapter("select * from PackageVersions", _conn);

      _cmdBuilder = new SqlCommandBuilder(_pkgVerAdapter);

      _pkgAdapter.Fill(_dataSet, "Packages");
      _pkgVerAdapter.Fill(_dataSet, "PackageVersions");

      var dr = new DataRelation(
        "fk_PackageVersions_Packages",
        _dataSet.Tables["Packages"].Columns["id"],
        _dataSet.Tables["PackageVersions"].Columns["package"]);

      _dataSet.Relations.Add(dr);

      _pkgBindingSource = new BindingSource {
        DataSource = _dataSet,
        DataMember = "Packages"
      };

      _pkgVerBindingSourcew = new BindingSource {
        DataSource = _pkgBindingSource,
        DataMember = "fk_PackageVersions_Packages"
      };

      pkgGridView.DataSource = _pkgBindingSource;
      pkgVerGridView.DataSource = _pkgVerBindingSourcew;
    }


    void Form1_Load(object sender, EventArgs e) => _initialize();

    void updateBtn_Click(object sender, EventArgs e) => _pkgVerAdapter.Update(_dataSet, "PackageVersions");
    void refreshBtn_Click(object sender, EventArgs e) => _initialize();
  }
}
