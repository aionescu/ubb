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
create or alter procedure addOSIApprovalColum as
  alter table Licenses
  add isOSIApproved bit
go

create or alter procedure removeOSIApprovalColum as
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
