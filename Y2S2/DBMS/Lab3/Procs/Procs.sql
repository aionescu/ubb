use MicroPackageManager
go

set XACT_ABORT on
go

create or alter procedure logSuccess @action nvarchar(100) as
  insert into Logs values (@action, CURRENT_TIMESTAMP, 1, NULL)
go

create or alter procedure logFailure @action nvarchar(100), @errorMessage nvarchar(200) as
  insert into Logs values (@action, CURRENT_TIMESTAMP, 0, @errorMessage);
go

create or alter procedure logFailureThrow @action nvarchar(100), @errorMessage nvarchar(200) as
  exec logFailure @action, @errorMessage;
  throw 50005, @errorMessage, 1
go

create or alter procedure addMaintainer @username nvarchar(50), @email nvarchar(50) as
  if exists (select * from Maintainers where username = @username)
    exec logFailureThrow 'addMaintainer', 'Duplicate maintainer username'

  if exists (select * from Maintainers where email = @email)
    exec logFailureThrow 'addMaintainer', 'Duplicate maintainer email'

  insert into Maintainers values (@username, @email)
  exec logSuccess 'addMaintainer'
go

create or alter procedure addPackage @name nvarchar(50), @description nvarchar(200) as
  if exists (select * from Packages where name = @name)
    exec logFailureThrow 'addPackage', 'Duplicate package name'
  else begin
    insert into Packages values (@name, @description)
    exec logSuccess 'addPackage'
  end
go

create or alter procedure addPackageMaintainer @maintainerUsername nvarchar(50), @packageName nvarchar(50) as
  declare @maintainerID int
  set @maintainerID = (select id from Maintainers where username = @maintainerUsername)

  if @@ROWCOUNT = 0
    exec logFailureThrow 'addPackageMaintainer', 'Inexistent maintainer'

  declare @packageID int
  set @packageID = (select id from Packages where name = @packageName)

  if @@ROWCOUNT = 0
    exec logFailureThrow 'addPackageMaintainer', 'Inexistent package'

  if exists (select * from PackageMaintainers where package = @packageID and maintainer = @maintainerID)
    exec logFailureThrow 'addPackageMaintainer', 'Duplicate package maintainer'

  insert into PackageMaintainers values (@packageID, @maintainerID)
  exec logSuccess 'addPackageMaintainer'
go

create or alter procedure addMaintainerAndPackage_onFailRollback
  @maintainerUsername nvarchar(50), @maintainerEmail nvarchar(50),
  @packageName nvarchar(50), @packageDescription nvarchar(200)
as
  begin tran
  begin try
    exec addMaintainer @maintainerUsername, @maintainerEmail
    exec addPackage @packageName, @packageDescription
    exec addPackageMaintainer @maintainerUsername, @packageName
    commit tran
    exec logSuccess 'addMaintainerAndPackage_onFailRollback'
  end try
  begin catch
    rollback tran
    exec logFailure 'addMaintainerAndPackage_onFailRollback', 'Transaction failed'
  end catch
go

create or alter procedure addMaintainerAndPackage_onFailKeepGoing
  @maintainerUsername nvarchar(50), @maintainerEmail nvarchar(50),
  @packageName nvarchar(50), @packageDescription nvarchar(200)
as
  declare @keepGoing bit
  set @keepGoing = 1

  begin try
    exec addMaintainer @maintainerUsername, @maintainerEmail
  end try
  begin catch
    set @keepGoing = 0
  end catch

  begin try
    exec addPackage @packageName, @packageDescription
  end try
  begin catch
    set @keepGoing = 0
  end catch


  if (@keepGoing = 1)
    begin try
      exec addPackageMaintainer @maintainerUsername, @packageName
      exec logSuccess 'addMaintainerAndPackage_onFailKeepGoing'
    end try
    begin catch
      exec logFailure 'addMaintainerAndPackage_onFailKeepGoing', 'All 3 operations failed'
    end catch
go
