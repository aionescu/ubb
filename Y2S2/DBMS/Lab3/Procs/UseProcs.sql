select * from Maintainers
select * from Packages
select * from PackageMaintainers
select * from Logs

truncate table Maintainers
truncate table Packages
truncate table PackageMaintainers
truncate table Logs

exec addMaintainerAndPackage_onFailRollback 'alx', 'alx@email.com', 'pkg', 'pkgDesc'
exec addMaintainerAndPackage_onFailRollback 'alx2', 'alx2@email.com', 'pkg', 'pkgDesc'
exec addMaintainerAndPackage_onFailKeepGoing 'alx2', 'alx2@email.com', 'pkg', 'pkgDesc'
