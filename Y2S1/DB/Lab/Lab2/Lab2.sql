use PackageManager
go

/*
insert into Tags values
  ('TextEditor')

insert into Users values
  ('someuser2000', 'Joe Smih', 'joesmithhh@gmail.org', 'AAAASFSF', 'ASGASGDASGDGD')

insert into Packages values
  ('coolpkg', 8, 'A cool package', 'https://github.com/someuser2000/coolpkg', 4)
  , ('java-openjdk', 10, 'Open JDK', 'https://oracle.com/git/openjdk', 4)
  -- ^ Violates referential integrity

insert into PackageVersions values
  (6, '0.0.0.0', 'https://pkgman.com/pkg/ghc/0.0.0.0')

update Users
set fullName = 'Joe Smith Jr.'
where loginName = 'someuser2000'

update Tags
set name = 'Text Editor'
where name = 'TextEditor'

update Packages
set description = 'The coolest progrma ever'
where name = 'coolpkg'

update Packages
set maintainer = null
where name = 'coolpkg'

delete from Users where loginName = 'someuser2000'
delete from Tags where name = 'Text Editor'
*/

-- a)
-- Select all package and distro names
select name from Packages
union all
select name from Distributions
order by name

-- Select all packages that have no maintainer or no source repository,
-- and all distros that have no maintainer
select name from Packages
where maintainer is null or sourceRepo is null
union
select name from Distributions
where maintainer is null

-- b)
-- Select users that maintain both packages and distros
-- This query also satisfies e)
select loginName, fullName from Users
where id in (
  select maintainer from Packages
  intersect
  select maintainer from Distributions
)

-- Selects name clashes between distros and packages
select name from Packages
intersect
select name from Distributions

-- c)
-- Select users that maintain ONLY packages
-- This query also satisfies e)
select loginName, fullName from Users
where id in (
  select maintainer from Packages
  except
  select maintainer from Distributions
)
order by loginName

-- Select users that don't maintain packages
select loginName, fullName from Users
where id not in (
  select maintainer from Packages
)
order by loginName

-- d)
-- Select tags that appear on packages with pre-release versions
-- This query also satisfies e)
select name from tags
where id in (
  select tag from PackagesTags
  inner join PackageVersions
  on PackagesTags.package = PackageVersions.package and PackageVersions.version like '0.%'
)

-- Select all users, and any packages or distros they may maintain
select Users.loginName, Packages.name, Distributions.name from Users
left join Packages
on Users.id = Packages.maintainer
left join Distributions
on Users.id = Distributions.maintainer

-- Select all packages and their licenses
select Packages.name, Licenses.name from Licenses
right join Packages
on Packages.license = Licenses.id

-- Select all distros and their packages
select
  Distributions.name,
  DistributionVersions.version,
  Packages.name,
  DistributionsPackages.packageVersion
from DistributionVersions
full join DistributionsPackages
on
  DistributionVersions.distribution = DistributionsPackages.distribution
  and DistributionVersions.version = DistributionsPackages.distributionVersion
left join Distributions
on DistributionVersions.distribution = Distributions.id
left join Packages
on DistributionsPackages.package = Packages.id

-- f)
-- Select all packages without dependencies
select distinct name from Packages
where not exists (
  select * from PackageDependencies
  where package = id
)

-- Select packages which appear in a distro
select distinct name from Packages
where exists (
  select * from DistributionsPackages
  where package = id
)

-- g)

-- Select top 2 packages by number of versions
select top 2 Packages.name, versionCount from (
  select package, count(*) as versionCount from PackageVersions
  group by package
) as VersionCounts
left join Packages
on package = Packages.id
order by versionCount desc

-- Select the average number of versions per distro
select avg(versionCount) as averageVersionCount from (
  select distribution, count(version) as versionCount from DistributionVersions
  group by distribution
) as VersionAvgs

-- h)

-- Select all licenses used by at least 3 packages
select Licenses.name, packageCount from (
  select license, count(id) as packageCount from Packages
  group by license
  having count(id) >= 1
) as LicenseCounts
left join Licenses
on license = Licenses.id

-- i)

-- Select all unused tags

select distinct name from Tags
where id not in (
  select tag from PackagesTags
)
order by name

-- Select all used licenses (First w/ ANY, then w/ IN)

select name from Licenses
where id = any (
  select license from Packages
)
order by name

select name from Licenses
where id in (
  select license from Packages
)
order by name

-- Select total count of package and distro versions
select
  (select count(*) from PackageVersions)
  + (select count(*) from DistributionVersions)
as totalVersionCount
