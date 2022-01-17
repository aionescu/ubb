namespace Lab2WinForm {
  using System;
  using System.Configuration;
  using System.Data;
  using System.Data.SqlClient;
  using System.Windows.Forms;

  public partial class Form1: Form {
    string _parentTable, _parentTableID, _childTable, _childTableFK;

    SqlConnection _conn;
    DataSet _dataSet;
    SqlDataAdapter _parentAdapter, _childAdapter;
    BindingSource _parentBindingSource, _childBindingSource;
    SqlCommandBuilder _cmdBuilder;

    public Form1() => InitializeComponent();

    void _loadConfig() {
      _parentTable = ConfigurationManager.AppSettings.Get("ParentTable") ?? "Packages";
      _parentTableID = ConfigurationManager.AppSettings.Get("ParentTableID") ?? "id";

      _childTable = ConfigurationManager.AppSettings.Get("ChildTable") ?? "PackageVersions";
      _childTableFK = ConfigurationManager.AppSettings.Get("ChildTableFK") ?? "package";
    }

    void _initialize() {
      _loadConfig();

      _conn = new SqlConnection(@"Data Source = localhost\SQLEXPRESS; Initial Catalog = PackageManager; Integrated Security = SSPI;");
      _dataSet = new DataSet();

      _parentAdapter = new SqlDataAdapter($"select * from {_parentTable}", _conn);
      _childAdapter = new SqlDataAdapter($"select * from {_childTable}", _conn);

      _cmdBuilder = new SqlCommandBuilder(_childAdapter);

      _parentAdapter.Fill(_dataSet, _parentTable);
      _childAdapter.Fill(_dataSet, _childTable);

      var fk = $"fk_{_childTable}_{_parentTable}";

      var dataRel = new DataRelation(fk,
        _dataSet.Tables[_parentTable].Columns[_parentTableID],
        _dataSet.Tables[_childTable].Columns[_childTableFK]
      );

      _dataSet.Relations.Add(dataRel);

      _parentBindingSource = new BindingSource {
        DataSource = _dataSet,
        DataMember = _parentTable
      };

      _childBindingSource = new BindingSource {
        DataSource = _parentBindingSource,
        DataMember = fk
      };

      pkgGridView.DataSource = _parentBindingSource;
      pkgVerGridView.DataSource = _childBindingSource;

      parentLbl.Text = _parentTable;
      childLbl.Text = _childTable;

      _conn.Open();
    }

    void Form1_Load(object sender, EventArgs e) => _initialize();

    void updateBtn_Click(object sender, EventArgs e) => _childAdapter.Update(_dataSet, _childTable);

    void refreshBtn_Click(object sender, EventArgs e) {
      _dataSet.Clear();
      _parentAdapter.Fill(_dataSet, _parentTable);
      _childAdapter.Fill(_dataSet, _childTable);
    }
  }
}
