# Changed data capture

Change data capture records insert, update, and delete activity that is applied to a table.
This makes the details of the changes available in an easily consumed relational format.
Column information and the metadata that is required to apply the changes to a target environment
is captured for the modified rows and stored in change tables that mirror the column structure of
the tracked source tables. Table-valued functions are provided to allow systematic access to the
change data by consumers.

Before a capture instance can be created for individual tables, a member of the `sysadmin` fixed server
role must first enable the database for change data capture. This is done by running the stored
procedure `sys.sp_cdc_enable_db` in the database context.

Example:

```sql
use PackageManager
go

exec sys.sp_cdc_enable_db
go
```

Disabling CDC is done in a similar manner, by running the `sys.sp_cdc_disable_db` stored procedure:

```sql
use PackageManager
go

exec sys.sp_cdc_disable_db
go
```

After a database has been enabled for change data capture, members of the `db_owner` fixed database role
can create a capture instance for individual source tables by using the stored procedure `sys.sp_cdc_enable_table`.

For example:

```sql
use PackageManager
go

exec sys.sp_cdc_enable_table
  @source_schema = N'dbo',
  @source_name = N'Packages',
  @role_name = NULL,
  @supports_net_changes = 1
go
```

Disabling CDC for a particular table can be done by using the stored procedure `sys.sp_cdc_disable_table`.

```sql
use MyDB
go
exec sys.sp_cdc_disable_table
  @source_schema = N'dbo',
  @source_name   = N'Packages',
  @capture_instance = N'dbo_Packages'
go
```
