use MicroPackageManager
go

set XACT_ABORT on
go

create or alter procedure logSuccess @operation nvarchar(10), @tableName nvarchar(50) as
  insert into Logs values (@operation, @tableName, CURRENT_TIMESTAMP, 1, NULL)
go

create or alter procedure logFailure @operation nvarchar(10), @tableName nvarchar(50), @errorMessage nvarchar(200) as
  insert into Logs values (@operation, @tableName, CURRENT_TIMESTAMP, 0, @errorMessage);
  throw 50005, @errorMessage, 1
go

create or alter procedure addPackage @name nvarchar(50), @description nvarchar(200) as
  if exists (select * from Packages where name = @name)
    exec logFailure 'insert', 'Packages', 'Duplicate package name'
  else begin
    insert into Packages values (@name, @description)
    exec logSuccess 'insert', 'Packages'
  end
go

create or alter procedure addMaintainer @username nvarchar(50), @email nvarchar(50) as
  if exists (select * from Maintainers where username = @username)
    exec logFailure 'insert', 'Maintainers', 'Duplicate maintainer username'

  if exists (select * from Maintainers where email = @email)
    exec logFailure 'insert', 'Maintainers', 'Duplicate maintainer email'

  insert into Maintainers values (@username, @email)
  exec logSuccess 'insert', 'Maintainers'
go

create or alter procedure addPackageMaintainer @packageName nvarchar(50), @maintainerUsername nvarchar(50) as
  declare @packageID int
  set @packageID = (select id from Packages where name = @packageName)

  if @@ROWCOUNT = 0
    exec logFailure 'insert', 'PackageMaintainers', 'Inexistent package'

  declare @maintainerID int
  set @maintainerID = (select id from Maintainers where username = @maintainerUsername)

  if @@ROWCOUNT = 0
    exec logFailure 'insert', 'PackageMaintainers', 'Inexistent maintainer'

  if exists (select * from PackageMaintainers where package = @packageID and maintainer = @maintainerID)
    exec logFailure 'insert', 'PackageMaintainers', 'Duplicate package maintainer'

  insert into PackageMaintainers values (@packageID, @maintainerID)
  exec logSuccess 'insert', 'PackageMaintainers'
go
