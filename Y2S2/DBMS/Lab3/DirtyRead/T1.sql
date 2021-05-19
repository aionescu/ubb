begin tran

update Maintainers
set username = 'ionescu'
where id = 1

waitfor delay '00:00:10'
rollback tran

-- update Maintainers
-- set username = 'alex'
-- where id = 1
