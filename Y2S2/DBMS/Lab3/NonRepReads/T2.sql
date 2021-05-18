set tran isolation level read committed
-- set tran isolation level repeatable read

begin tran
select * from Maintainers

waitfor delay '00:00:15'
select * from Maintainers

commit tran
