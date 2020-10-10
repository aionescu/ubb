create database PackageManager;
go

use PackageManager;
go

create table Users
  ( userID int
  , username nvarchar(50)
  , fullName nvarchar(100)
  , email nvarchar(50)
  , passwordSalt nvarchar(20)
  , passwordHash nvarchar(64)
  primary key (userID)
  );

create table Packages
  ( packageID int
  , packageName nvarchar(50)
  , maintainer int
  , decription nvarchar(500)
  , sourceRepo nvarchar(200)
  , license int
  primary key (packageID)
  );

create table PackageVersions
  ( packageVersionID int
  , packageID int
  , packageVersion nvarchar(50)
  primary key (packageVersionID)
  )

create table PackageTags
  ( tagID int
  , tagName nvarchar(20)
  primary key (tagID)
  );

create table Licenses
  ( licenseID int
  , licenseName nvarchar(50)
  primary key (licenseID)
  );

go
