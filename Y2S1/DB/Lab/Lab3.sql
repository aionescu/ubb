-- a)
create or alter procedure extendTagNameColumn as
  alter table Tags
  alter column name nvarchar(50)
go

create or alter procedure shrinkTagNameColumn as
  alter table Tags
  alter column name nvarchar(20)
go

-- b)
create or alter procedure addOSIApprovalColumn as
  alter table Licenses
  add isOSIApproved bit
go

create or alter procedure removeOSIApprovalColumn as
  alter table Licenses
  drop column isOSIApproved
go

-- c)
create or alter procedure addDefaultPackageDescription as
  alter table Packages
  add constraint df_description
  default 'No description.' for decription
go

create or alter procedure removeDefaultPackageDescription as
  alter table Packages
  drop constraint df_description
go

-- d)
create or alter procedure makeLoginNamePrimaryKey as
  alter table Users
  add constraint pk_loginName primary key (loginName)
go

create or alter procedure removeLoginNamePrimaryKey as
  alter table Users
  drop constraint pk_loginName
go

-- e)
create or alter procedure makePackageNameCandidateKey as
  alter table Packages
	add constraint uk_name unique (name)
go

create or alter procedure removePackageNameCandidateKey as
	alter table Packages
	drop constraint uk_name
go

-- g)
create or alter procedure createPackageAliasesTabel as
  create table PackageAliases
    ( id int not null primary key identity
    , package int not null
    , alias nvarchar(50)
    )
go

exec createPackageAliasesTabel
go

create or alter procedure dropPackageAliasesTabel as
	drop table PackageAliases
go

-- f)
create or alter procedure addPackageAliasForeignKey as
	alter table PackageAliases
	add constraint fk_package foreign key (package) references Packages(id)
go

create or alter procedure removePackageAliasForeignKey as
	alter table PackageAliases
	drop constraint fk_package
go

-- Tables for handling schema versioning

create table CurrentVersion(currentVersion int)
insert into CurrentVersion values (0)

create table Versions(version int, doProc nvarchar(100), undoProc nvarchar(100))
insert into Versions values
  (1, 'extendTagNameColumn', 'shrinkTagNameColumn')
, (2, 'addOSIApprovalColumn', 'removeOSIApprovalColumn')
go

create or alter procedure goToVersion @version int as begin
  declare @crrVersion int
  set @crrVersion = (select currentVersion from CurrentVersion)

	if @version < 0 or @version > (select COUNT(*) from Versions) begin
		raiserror ('Invalid version.', 10, 0)
		return
	end

  if @version = @crrVersion begin
		return 
	end

	if @version > @crrVersion begin
		declare versionCursor cursor for
			select doProc from Versions
			where version <= @version and version > @crrVersion
			order by version
	end
	else begin
		declare versionCursor cursor for
			select undoProc from Versions
			where version > @version and version <= @crrVersion
			order by version desc
	end

	declare @proc nvarchar(100)
	open versionCursor
	fetch next from versionCursor into @proc

	while @@FETCH_STATUS = 0 begin
		exec @proc
		fetch next from versionCursor into @proc
	end
  
	close versionCursor
	deallocate versionCursor

	update CurrentVersion
	set currentVersion = @version
end
go
