if exists (select name from master.sys.databases where name = N'PackageManager')
  drop database PackageManager

go

create database PackageManager
go

use PackageManager
go

create table Tags
  ( id int not null primary key identity
  , name nvarchar(20) not null
  )

create table Licenses
  ( id int not null primary key identity
  , name nvarchar(50) not null
  , infoURI nvarchar(200) not null
  )

create table Users
  ( id int not null primary key identity
  , loginName nvarchar(50) not null
  , fullName nvarchar(100) not null
  , email nvarchar(50) not null
  , passwordSalt nvarchar(20) not null
  , passwordHash nvarchar(64) not null
  )

create table Packages
  ( id int not null primary key identity
  , name nvarchar(50) not null
  , maintainer int foreign key references Users(id)
  , decription nvarchar(500)
  , sourceRepo nvarchar(200)
  , license int not null foreign key references Licenses(id)
  )

create table PackageVersions
  ( package int not null foreign key references Packages(id)
  , version nvarchar(50) not null
  , tarballURI nvarchar(200) not null
  , primary key (package, version)
  )

create table PackagesTags
  ( package int not null foreign key references Packages(id)
  , tag int not null foreign key references Tags(id)
  , primary key (package, tag)
  )

create table PackageDependencies
  ( package int not null
  , packageVersion nvarchar(50) not null
  , dependency int not null
  , dependencyVersion nvarchar(50) not null
  , primary key (package, packageVersion, dependency, dependencyVersion)
  , foreign key (package, packageVersion) references PackageVersions(package, version)
  , foreign key (dependency, dependencyVersion) references PackageVersions(package, version)
  )

create table Distributions
  ( id int not null primary key identity
  , name nvarchar(50) not null
  , maintainer int foreign key references Users(id)
  )

create table DistributionVersions
  ( distribution int not null foreign key references Distributions(id)
  , version nvarchar(50) not null
  , primary key (distribution, version)
  )

create table DistributionsPackages
  ( distribution int not null
  , distributionVersion nvarchar(50) not null
  , package int not null
  , packageVersion nvarchar(50) not null
  , primary key (distribution, distributionVersion, package, packageVersion)
  , foreign key (distribution, distributionVersion) references DistributionVersions(distribution, version)
  , foreign key (package, packageVersion) references PackageVersions(package, version)
  )

go

insert into Tags values
  ('Tool')
, ('GUI')
, ('Web Browser')
, ('Compiler')
, ('Other')

insert into Licenses values
  ('GPL-2', 'https://opensource.org/licenses/GPL-2.0')
, ('GPL-3', 'https://opensource.org/licenses/GPL-3.0')
, ('BSD-3', 'https://opensource.org/licenses/BSD-3-Clause')
, ('MIT', 'https://opensource.org/licenses/MIT')
, ('Apache-2', 'https://opensource.org/licenses/Apache-2.0')

-- Pretend the passwordSalt and passwordHash values are real hashes :-).
insert into Users values
  ('torvalds', 'Linus Torvalds', 'torvalds@osdl.org', 'ABCDEF', 'ABCDEFGHIJKL')
, ('rms', 'Richard M. Stallman', 'rms@gnu.org', 'GHIJKASDL', 'ASDFASASDADGDEGA')
, ('aionescu', 'Alex Ionescu', 'alxi.2001@gmail.com', 'GHIJKL', 'ASDFASDGDEGA')
, ('guido', 'Guido van Rossum', 'guido@something.com', 'ASDSAD', 'GDSGETGSAGF')
, ('simonpj', 'Simon Peyton Jones', 'simonpj@microsoft.com', 'KHUHHN', 'ESIKGUHESKI')
, ('arch-team', 'Arch Development Team', 'arch@something.com', 'AAAA', 'ASFAEGAEWEAG')
, ('nix-team', 'Nix Development Team', 'nix@something.com', 'ASDSAAG', 'ASFAGEEGESGEGES')

insert into Packages values
  ('linux', 1, 'A cool kernel', 'https://github.com/torvalds/linux', 1)
, ('coreutils', 2, 'Cool GNU utilities', 'https://gnu.org/coreutils.git', 2)
, ('tli', 3, 'Toy Language Interpreter', null, 4)
, ('python3', 4, 'A cool language', 'https://github.com/python/cpython', 4)
, ('ghc', 5, 'The Glorious Glasgow Haskell Compilation System', 'https://gitlab.haskell.org/ghc/ghc', 4)

insert into PackageVersions values
  (1, '1.0.0.0', 'https://pkgman.com/pkg/linux/1.0.0.0')
, (2, '1.0.0.0', 'https://pkgman.com/pkg/coreutils/1.0.0.0')
, (3, '0.0.0.0', 'https://pkgman.com/pkg/tli/1.0.0.0')
, (4, '1.0.0.0', 'https://pkgman.com/pkg/python3/1.0.0.0')
, (5, '0.1.0.0', 'https://pkgman.com/pkg/ghc/0.1.0.0')
, (5, '1.0.0.0', 'https://pkgman.com/pkg/ghc/1.0.0.0')
, (5, '1.1.0.0', 'https://pkgman.com/pkg/ghc/1.1.0.0')

insert into PackagesTags values
  (1, 5)
, (2, 5)
, (2, 1)
, (3, 4)
, (4, 4)
, (5, 4)
, (5, 1)

insert into PackageDependencies values
  (3, '0.0.0.0', 1, '1.0.0.0')
, (3, '0.0.0.0', 2, '1.0.0.0')
, (3, '0.0.0.0', 5, '1.0.0.0')
, (4, '1.0.0.0', 1, '1.0.0.0')
, (4, '1.0.0.0', 2, '1.0.0.0')
, (5, '1.0.0.0', 1, '1.0.0.0')
, (5, '1.0.0.0', 2, '1.0.0.0')
, (5, '1.1.0.0', 1, '1.0.0.0')
, (5, '1.1.0.0', 2, '1.0.0.0')

insert into Distributions values
  ('Arch', 6)
, ('Debian', null)
, ('Nix', 7)
, ('Stackage', 5)
, ('VoidLinux', null)

insert into DistributionVersions values
  (1, '1.0.0.0')
, (2, '1.0.0.0')
, (3, '1.0.0.0')
, (3, '1.1.0.0')
, (3, '2.0.0.0')
, (4, '1.0.0.0')
, (4, '1.1.0.0')
, (4, '1.1.0.2')

insert into DistributionsPackages values
  (1, '1.0.0.0', 1, '1.0.0.0')
, (1, '1.0.0.0', 2, '1.0.0.0')
, (2, '1.0.0.0', 1, '1.0.0.0')
, (2, '1.0.0.0', 2, '1.0.0.0')
, (3, '1.0.0.0', 1, '1.0.0.0')
, (3, '1.0.0.0', 2, '1.0.0.0')
, (3, '1.0.0.0', 5, '1.1.0.0')
, (4, '1.0.0.0', 1, '1.0.0.0')
, (4, '1.0.0.0', 2, '1.0.0.0')

go

use master
go
