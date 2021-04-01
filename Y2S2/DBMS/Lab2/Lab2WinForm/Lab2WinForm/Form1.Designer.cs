namespace Lab2WinForm {

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
      this.parentLbl = new System.Windows.Forms.Label();
      this.pkgGridView = new System.Windows.Forms.DataGridView();
      this.childLbl = new System.Windows.Forms.Label();
      this.pkgVerGridView = new System.Windows.Forms.DataGridView();
      this.updateBtn = new System.Windows.Forms.Button();
      this.refreshBtn = new System.Windows.Forms.Button();
      ((System.ComponentModel.ISupportInitialize)(this.pkgGridView)).BeginInit();
      ((System.ComponentModel.ISupportInitialize)(this.pkgVerGridView)).BeginInit();
      this.SuspendLayout();
      // 
      // parentLbl
      // 
      this.parentLbl.AutoSize = true;
      this.parentLbl.Font = new System.Drawing.Font("Fira Code", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
      this.parentLbl.Location = new System.Drawing.Point(13, 13);
      this.parentLbl.Name = "parentLbl";
      this.parentLbl.Size = new System.Drawing.Size(89, 20);
      this.parentLbl.TabIndex = 0;
      this.parentLbl.Text = "Packages";
      // 
      // pkgGridView
      // 
      this.pkgGridView.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize;
      this.pkgGridView.Location = new System.Drawing.Point(17, 37);
      this.pkgGridView.Name = "pkgGridView";
      this.pkgGridView.Size = new System.Drawing.Size(824, 150);
      this.pkgGridView.TabIndex = 1;
      // 
      // childLbl
      // 
      this.childLbl.AutoSize = true;
      this.childLbl.Font = new System.Drawing.Font("Fira Code", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
      this.childLbl.Location = new System.Drawing.Point(13, 254);
      this.childLbl.Name = "childLbl";
      this.childLbl.Size = new System.Drawing.Size(159, 20);
      this.childLbl.TabIndex = 2;
      this.childLbl.Text = "PackageVersions";
      // 
      // pkgVerGridView
      // 
      this.pkgVerGridView.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize;
      this.pkgVerGridView.Location = new System.Drawing.Point(17, 278);
      this.pkgVerGridView.Name = "pkgVerGridView";
      this.pkgVerGridView.Size = new System.Drawing.Size(824, 150);
      this.pkgVerGridView.TabIndex = 3;
      // 
      // updateBtn
      // 
      this.updateBtn.Font = new System.Drawing.Font("Fira Code", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
      this.updateBtn.Location = new System.Drawing.Point(17, 488);
      this.updateBtn.Name = "updateBtn";
      this.updateBtn.Size = new System.Drawing.Size(133, 31);
      this.updateBtn.TabIndex = 4;
      this.updateBtn.Text = "Update DB";
      this.updateBtn.UseVisualStyleBackColor = true;
      this.updateBtn.Click += new System.EventHandler(this.updateBtn_Click);
      // 
      // refreshBtn
      // 
      this.refreshBtn.Font = new System.Drawing.Font("Fira Code", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
      this.refreshBtn.Location = new System.Drawing.Point(156, 488);
      this.refreshBtn.Name = "refreshBtn";
      this.refreshBtn.Size = new System.Drawing.Size(133, 31);
      this.refreshBtn.TabIndex = 5;
      this.refreshBtn.Text = "Refresh";
      this.refreshBtn.UseVisualStyleBackColor = true;
      this.refreshBtn.Click += new System.EventHandler(this.refreshBtn_Click);
      // 
      // Form1
      // 
      this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
      this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
      this.ClientSize = new System.Drawing.Size(853, 580);
      this.Controls.Add(this.refreshBtn);
      this.Controls.Add(this.updateBtn);
      this.Controls.Add(this.pkgVerGridView);
      this.Controls.Add(this.childLbl);
      this.Controls.Add(this.pkgGridView);
      this.Controls.Add(this.parentLbl);
      this.Name = "Form1";
      this.Text = "Form1";
      this.Load += new System.EventHandler(this.Form1_Load);
      ((System.ComponentModel.ISupportInitialize)(this.pkgGridView)).EndInit();
      ((System.ComponentModel.ISupportInitialize)(this.pkgVerGridView)).EndInit();
      this.ResumeLayout(false);
      this.PerformLayout();

    }

    #endregion

    private System.Windows.Forms.Label parentLbl;
    private System.Windows.Forms.DataGridView pkgGridView;
    private System.Windows.Forms.Label childLbl;
    private System.Windows.Forms.DataGridView pkgVerGridView;
    private System.Windows.Forms.Button updateBtn;
    private System.Windows.Forms.Button refreshBtn;
  }
}

