alter database MicroPackageManager
set allow_snapshot_isolation on

waitfor delay '00:00:10'

begin tran

update Maintainers
set username = 'ionescu'
where id = 1

waitfor delay '00:00:10'
commit tran

-- update Maintainers
-- set username = 'alex'
-- where id = 1
