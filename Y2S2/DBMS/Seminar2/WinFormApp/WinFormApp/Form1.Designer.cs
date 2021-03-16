namespace WinFormApp {

  partial class Form1 {
    /// <summary>
    /// Required designer variable.
    /// </summary>
    private System.ComponentModel.IContainer components = null;

    /// <summary>
    /// Clean up any resources being used.
    /// </summary>
    /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
    protected override void Dispose(bool disposing) {
      if (disposing && (components != null)) {
        components.Dispose();
      }
      base.Dispose(disposing);
    }

    #region Windows Form Designer generated code

    /// <summary>
    /// Required method for Designer support - do not modify
    /// the contents of this method with the code editor.
    /// </summary>
    private void InitializeComponent() {
      this.label1 = new System.Windows.Forms.Label();
      this.dgvCustomers = new System.Windows.Forms.DataGridView();
      this.label2 = new System.Windows.Forms.Label();
      this.dgvOrders = new System.Windows.Forms.DataGridView();
      this.btnSaveData = new System.Windows.Forms.Button();
      this.northwindDataSet = new WinFormApp.NorthwindDataSet();
      ((System.ComponentModel.ISupportInitialize)(this.dgvCustomers)).BeginInit();
      ((System.ComponentModel.ISupportInitialize)(this.dgvOrders)).BeginInit();
      ((System.ComponentModel.ISupportInitialize)(this.northwindDataSet)).BeginInit();
      this.SuspendLayout();
      // 
      // label1
      // 
      this.label1.AutoSize = true;
      this.label1.Font = new System.Drawing.Font("Fira Code", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
      this.label1.Location = new System.Drawing.Point(13, 13);
      this.label1.Name = "label1";
      this.label1.Size = new System.Drawing.Size(99, 20);
      this.label1.TabIndex = 0;
      this.label1.Text = "Customers";
      // 
      // dgvCustomers
      // 
      this.dgvCustomers.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize;
      this.dgvCustomers.Location = new System.Drawing.Point(13, 37);
      this.dgvCustomers.Name = "dgvCustomers";
      this.dgvCustomers.Size = new System.Drawing.Size(1038, 150);
      this.dgvCustomers.TabIndex = 1;
      // 
      // label2
      // 
      this.label2.AutoSize = true;
      this.label2.Font = new System.Drawing.Font("Fira Code", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
      this.label2.Location = new System.Drawing.Point(13, 238);
      this.label2.Name = "label2";
      this.label2.Size = new System.Drawing.Size(69, 20);
      this.label2.TabIndex = 2;
      this.label2.Text = "Orders";
      // 
      // dgvOrders
      // 
      this.dgvOrders.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize;
      this.dgvOrders.Location = new System.Drawing.Point(13, 262);
      this.dgvOrders.Name = "dgvOrders";
      this.dgvOrders.Size = new System.Drawing.Size(1038, 150);
      this.dgvOrders.TabIndex = 3;
      // 
      // btnSaveData
      // 
      this.btnSaveData.Font = new System.Drawing.Font("Fira Code", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
      this.btnSaveData.Location = new System.Drawing.Point(13, 462);
      this.btnSaveData.Name = "btnSaveData";
      this.btnSaveData.Size = new System.Drawing.Size(158, 30);
      this.btnSaveData.TabIndex = 4;
      this.btnSaveData.Text = "Save Data";
      this.btnSaveData.UseVisualStyleBackColor = true;
      this.btnSaveData.Click += new System.EventHandler(this.btnSaveData_Click);
      // 
      // northwindDataSet
      // 
      this.northwindDataSet.DataSetName = "NorthwindDataSet";
      this.northwindDataSet.SchemaSerializationMode = System.Data.SchemaSerializationMode.IncludeSchema;
      // 
      // Form1
      // 
      this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
      this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
      this.ClientSize = new System.Drawing.Size(1063, 644);
      this.Controls.Add(this.btnSaveData);
      this.Controls.Add(this.dgvOrders);
      this.Controls.Add(this.label2);
      this.Controls.Add(this.dgvCustomers);
      this.Controls.Add(this.label1);
      this.Name = "Form1";
      this.Text = "Form1";
      this.Load += new System.EventHandler(this.Form1_Load);
      ((System.ComponentModel.ISupportInitialize)(this.dgvCustomers)).EndInit();
      ((System.ComponentModel.ISupportInitialize)(this.dgvOrders)).EndInit();
      ((System.ComponentModel.ISupportInitialize)(this.northwindDataSet)).EndInit();
      this.ResumeLayout(false);
      this.PerformLayout();

    }

    #endregion

    private System.Windows.Forms.Label label1;
    private NorthwindDataSet northwindDataSet;
    private System.Windows.Forms.DataGridView dgvCustomers;
    private System.Windows.Forms.Label label2;
    private System.Windows.Forms.DataGridView dgvOrders;
    private System.Windows.Forms.Button btnSaveData;
  }
}

