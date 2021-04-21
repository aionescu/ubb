if exists (select name from master.sys.databases where name = N'MicroPackageManager')
  drop database MicroPackageManager
go

create database MicroPackageManager
go

use MicroPackageManager
go

create table Logs
  ( id int not null primary key identity
  , operation nvarchar(10) not null
  , tableName nvarchar(50) not null
  , timestamp datetime2 not null
  , successful bit not null
  , errorMessage nvarchar(200)
  )

create table Maintainers
  ( id int not null primary key identity
  , username nvarchar(50) not null unique
  , email nvarchar(50) not null unique
  )

create table Packages
  ( id int not null primary key identity
  , name nvarchar(50) not null unique
  , description nvarchar(200)
  )

create table PackageMaintainers
  ( package int not null foreign key references Packages(id)
  , maintainer int not null foreign key references Maintainers(id)
  , primary key (package, maintainer)
  )
